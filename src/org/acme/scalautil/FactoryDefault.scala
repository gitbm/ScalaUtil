package org.acme.scalautil

//class FactoryDefault[T : ClassManifest] extends Factory[T] {
//	private val cons = classManifest[T].erasure.asInstanceOf[Class[T]].getConstructor()
//    def create(meta : Option[Either[String, AnyRef]]): T = cons.newInstance()
//}
//
//object FactoryDefault {
//	def apply[T : ClassManifest] = new FactoryDefault[T]
//}

class FactoryDefault extends Factory {
	
	import scala.collection._
	private val classMap = mutable.Map[Class[_], Class[_]]()
	private val injectionMap = mutable.Map[Class[_], Any]()
	
	private def findClass(base : Class[_]) : Class[_] = {
		classMap.getOrElse(base, base)
	}
	def create[T : ClassManifest]: T = {
		findClass(classManifest[T].erasure).getConstructor().newInstance().asInstanceOf[T]
	}
	def create[T : ClassManifest, A : ClassManifest](arg : A) = { 
    	findClass(classManifest[T].erasure).getConstructor(classManifest[A].erasure).newInstance(arg.asInstanceOf[Object]).asInstanceOf[T]
    }
   	def create[T : ClassManifest, A1 : ClassManifest, A2 : ClassManifest](arg1 : A1, arg2 : A2) = {
    	findClass(classManifest[T].erasure).getConstructor(
    		classManifest[A1].erasure, classManifest[A2].erasure).newInstance(arg1.asInstanceOf[Object], arg2.asInstanceOf[Object]).asInstanceOf[T]
   	}
   	
	def defineImpl[T : ClassManifest, I <: T : ClassManifest] : Unit = {
		synchronized {
			classMap(classManifest[T].erasure) = classManifest[I].erasure
		}
	}
	
	// First injection will create object if default constructor available
	def inject[T : ClassManifest] : T = {
		synchronized {
			injectionMap.getOrElseUpdate(classManifest[T].erasure, create[T]).asInstanceOf[T]
		}
	}
	
	// Where default constructor not desirable or when timing of creation needs to be controlled register 
	// the object to be injected first
	def register[T : ClassManifest](obj : T) : Unit = {
		synchronized {
			injectionMap(classManifest[T].erasure) = obj
		}
	}

}