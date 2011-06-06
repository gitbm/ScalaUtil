package org.acme.scalautil

import scala.collection.mutable. { Map => MutableMap }

object PropertiesFromFile {
	val SYSPROP_PROPERTY_FILE = "propertyFile"
	val DEFAULT_PROPERTY_FILE_NAME = "runtime.properties"
}

import PropertiesFromFile._
class PropertiesFromFile extends Properties {
	private var init = false
	private var defaultProperties : Map[String, String] = _
	private var runtimeProperties = MutableMap[String, String]()

	def initialise(initString : Option[String], defaults : Map[String, String]) = {

		defaultProperties = defaults

		// Use initString as filename else look for it in System Property "propertyFile" else use "runtime.properties"
		val propFileName = initString.getOrElse(
			Option(System.getProperty(SYSPROP_PROPERTY_FILE)).getOrElse(DEFAULT_PROPERTY_FILE_NAME))

		try {
			val pFile = new java.io.FileInputStream(propFileName)
			val jRuntimeProperties = new java.util.Properties
			try {
				jRuntimeProperties.load(pFile)
			}
			finally {
				pFile.close()
			}
			val it = jRuntimeProperties.entrySet().iterator()
			while (it.hasNext()) {
				val e = it.next()
				runtimeProperties += (e.getKey().asInstanceOf[String] -> e.getValue().asInstanceOf[String])
			}
		}
		catch {
			case _ => throw new RuntimeException("No Properties file found.  Hint: use -DpropertyFile (runtime.properties is the default)")
		}
		init = true
		this
	}

	def get(name : String) : Option[String] = {
		if (!init) {initialise(None); init = true }
		runtimeProperties.get(name).orElse(defaultProperties.get(name))
	}

	def addDefaults(defaults : Map[String, String]) = { defaultProperties ++= defaults; this }

}