package org.acme.scalautil

import org.acme.scalautil.Factory._

trait Properties {
	def initialise(initString : Option[String], defaults : Map[String, String] = Map()) : Properties
	def addDefaults(defaults : Map[String, String]) : Properties
	def get(name : String) : Option[String]
	
	def getOrElse(name : String, elseValue: String) = get(name).getOrElse(elseValue)
	def getMandatory(name : String) = get(name).getOrElse(throw PropertyMissingException("Mandatory property (" + name + ") was not provided"))
}

case class PropertyMissingException(msg : String) extends RuntimeException(msg)
