(ns bacnet-io-creator.core
  (:gen-class :main true)
  (:use [seesaw.core]
        [seesaw.mig :only (mig-panel)]
        [seesaw.chooser :only (choose-file)]
        [seesaw.swingx :only (hyperlink)])
  (:require [clojure-csv.core :as csv]))

(import 'java.net.InetSocketAddress)
(import 'java.util.ArrayList)
(import 'java.util.List)
(import '(com.serotonin.bacnet4j 
          LocalDevice 
          RemoteDevice 
          service.acknowledgement.AcknowledgementService 
          service.acknowledgement.CreateObjectAck
          service.acknowledgement.ReadPropertyAck
          service.confirmed.ConfirmedRequestService
          service.confirmed.CreateObjectRequest
          service.confirmed.DeleteObjectRequest
          service.confirmed.ReadPropertyConditionalRequest
          service.confirmed.ReadPropertyMultipleRequest
          service.confirmed.ReadPropertyRequest
          service.confirmed.WritePropertyMultipleRequest
          service.confirmed.WritePropertyRequest
          service.unconfirmed.WhoIsRequest
          type.constructed.Address
          type.constructed.Destination
          type.constructed.EventTransitionBits
          type.constructed.PriorityArray
          type.constructed.PropertyReference
          type.constructed.PropertyValue
          type.constructed.ReadAccessSpecification
          type.constructed.Recipient
          type.constructed.SequenceOf
          type.constructed.WriteAccessSpecification
          type.enumerated.EngineeringUnits
          type.enumerated.ObjectType
          type.enumerated.PropertyIdentifier
          type.enumerated.Segmentation
          type.primitive.CharacterString
          type.primitive.ObjectIdentifier
          type.primitive.Real
          type.primitive.UnsignedInteger
          util.PropertyReferences))

(defn get-current-directory []
  (. (java.io.File. ".") getAbsolutePath))

(defn get-broadcast-address
  "Return the broadcast address as a string"
  []
  (clojure.string/join "."
                       (concat
                        (take 3 (clojure.string/split
                                 (.getHostAddress
                                  (java.net.InetAddress/getLocalHost))
                                 #"\."))
                        ["255"])))

(defn new-local-device
  "Return a new configured BACnet local device . (A device is required
to communicate over the BACnet network.). To terminate it, use the
java method `terminate'."
  [& {:keys [device-id broadcast-address port local-address]
      :or {device-id 1337
           broadcast-address (get-broadcast-address)
           port 47808
           local-address nil}}]
  (let [ld (LocalDevice. device-id broadcast-address local-address)]
    (-> ld (.setMaxReadMultipleReferencesNonsegmented 20))
    (.setPort ld port)
    ld))

(defmacro with-local-device
  "Initialize a local BACnet device, execute body and terminate the
  local device. Insure that the local device won't survive beyond its
  utility and lock a port. Check with-local-device-init for the config-map."
  [[device-binding device] & body]
  `(let [~device-binding ~device]
     (.initialize ~device-binding)
     (try ~@body
          (finally (.terminate ~device-binding)))))


(defn filter-type
  "For a given string (AI, BI, AO, BO), return the object-type object"
  [object-type-string]
  (let [string (clojure.string/lower-case object-type-string)
        test-fn (fn [[arg1 arg2] test-string]
                  (or (= arg1 test-string)
                      (= arg2 test-string)))]
    (cond (test-fn ["ai" "ea"] string)
          ObjectType/analogInput
          (test-fn ["bi" "eb"] string)
          ObjectType/binaryInput
          (test-fn ["ao" "sa"] string)
          ObjectType/analogOutput
          (test-fn ["bo" "sb"] string)
          ObjectType/binaryOutput)))

          
(defn create-io-request
  "Create a create-object-request to a remote BACnet device"
  [io-type io-instance io-name io-description]
  (let [type (filter-type io-type)
        additionnal-properties
        (when (= type ObjectType/binaryOutput)
          (PropertyValue. PropertyIdentifier/minimumOffTime ;; set a minimum off-time
                          (com.serotonin.bacnet4j.type.primitive.UnsignedInteger. 60)))
        oid (ObjectIdentifier. type (Integer/parseInt io-instance))]
    (CreateObjectRequest. oid
                          (SequenceOf.
                           (ArrayList.
                            (remove #(= nil %)
                                    [(PropertyValue.
                                      PropertyIdentifier/objectName
                                      (CharacterString. io-name))
                                     (PropertyValue.
                                      PropertyIdentifier/description
                                      (CharacterString. io-description))
                                     additionnal-properties]))))))


(defn create-io-requests-from-file [file-path]
  (let [file (slurp file-path)]
    (for [io-info (csv/parse-csv file :delimiter \tab)]
      (apply create-io-request io-info))))


(defn send-requests
  [local-device remote-device-id requests]
  (.sendBroadcast local-device (WhoIsRequest.))
  (Thread/sleep 500)
  (let [rd (.getRemoteDevice local-device remote-device-id)]
    (doseq [rq requests]
      (.send local-device rd rq))))

(defn get-remote-devices-list []
  (with-local-device [ld (new-local-device {})]
    (.sendBroadcast ld (WhoIsRequest.))
    (Thread/sleep 500)
    (for [rd (.getRemoteDevices ld)]
      (.getInstanceNumber rd))))

(defn send-requests-from-file [file-path remote-device-id]
  (with-local-device [ld (new-local-device :port 47808)]
    (let [requests (create-io-requests-from-file file-path)]
      (try (send-requests ld remote-device-id requests)
           (catch Exception e (str "Error: IOs already exist, bad address?"))))))


;; to remake: too long and confusing!
(defn query-user []
  "Open a dialog window for the user. Return this map: {:devID
foo, :file-object foo, :port foo}"
  (native!)
  (invoke-later
   (let [b (button :text "Choose IO file")
         file-object (atom nil)
         filename (text :editable? false)
         create-b (button :text "Create!")
         hyperlink-source (hyperlink :uri "https://github.com/Frozenlock/bacnet-io-creator" :text "Source")
         stop-create-b
         (listen create-b :action
                 (fn [e] (when-let [path (.getAbsolutePath @file-object)]
                           (let [devID (Integer/parseInt (text (select (to-root e) [:#devID])))]
                             (if-let [err (send-requests-from-file path devID)]
                               (alert err)
                               (alert "Done!"))))))
         stop-b (listen b :action (fn [e] (let [file (choose-file :dir (get-current-directory))]
                                            (text! filename (.getName file))
                                            (reset! file-object file))))]
     (->
      (frame :title "BACnet IO creator"
             :content
             (mig-panel
              :constraints ["wrap 2"
                            "[shrink 0]20px[200, grow, fill]"
                            "[shrink 0]5px[]"]
              :items [[b "growx"] [filename]
                      ["Device ID"][(text :id :devID :text "10100")]
                      ["Remote port (default 47808):"] [(text :id :port :text "47808")]
                      [ :separator         "growx, wrap, span, gaptop 10"]
                      [create-b "growx"]
                      [hyperlink-source]]))
      (pack!)
      (show!)))))


(defn -main [& args]
  (query-user))