package org.acme.scalautil
package test


class FactoryMockito extends Factory {

	import scala.collection._
	private val injectionMap = mutable.Map[Class[_], Any]()

	def create[T : ClassManifest]: T = FactoryMockito.getMock[T]
	def create[T : ClassManifest, A : ClassManifest](arg : A) = FactoryMockito.getMock[T](List(arg.asInstanceOf[AnyRef]))
   	def create[T : ClassManifest, A1 : ClassManifest, A2 : ClassManifest](arg1 : A1, arg2 : A2) = {
    	FactoryMockito.getMock[T](List(arg1.asInstanceOf[AnyRef], arg2.asInstanceOf[AnyRef]))
	}
    
    def defineImpl[T : ClassManifest, I <: T : ClassManifest] : Unit = {}   
    
   	def inject[T : ClassManifest] : T = injectionMap.getOrElseUpdate(classManifest[T].erasure, create[T]).asInstanceOf[T]
	def register[T : ClassManifest](obj : T) : Unit = {}

}

object FactoryMockito {
	import scala.collection._
	import org.mockito._
	import org.mockito.Matchers._
	def apply() = new FactoryMockito
	
	case class MockWithArgs[T](val mockObj : T, var argList : List[AnyRef])

	private val mockMap  = mutable.Map[Class[_], mutable.Queue[FactoryMockito.MockWithArgs[_]]]()

	def createAndRegisterMock[T <: AnyRef : ClassManifest] : MockWithArgs[T] = {
		val mockClass = classManifest[T].erasure.asInstanceOf[Class[T]]
		val mockWithArgs = MockWithArgs(Mockito.mock(mockClass), Nil)
		var q = mockMap.getOrElseUpdate(mockClass, mutable.Queue[MockWithArgs[_]]()) += mockWithArgs
		mockWithArgs
	}
	
	def createMock[T <: AnyRef : ClassManifest] : T = createAndRegisterMock[T].mockObj
	def createMockWithArgs[T <: AnyRef : ClassManifest] : MockWithArgs[T] = createAndRegisterMock[T]
	
	def getMockWithArgs[T : ClassManifest] :  MockWithArgs[T] = {
		val mockClass = classManifest[T].erasure.asInstanceOf[Class[T]]
		val mockWithArgs = mockMap.get(mockClass)
		if (mockWithArgs.isDefined && !mockWithArgs.get.isEmpty)
			mockWithArgs.get.dequeue.asInstanceOf[MockWithArgs[T]]
		else
			MockWithArgs[T](Mockito.mock(mockClass), Nil)
	}
	def getMock[T : ClassManifest] : T = getMockWithArgs[T].mockObj
	def getMock[T : ClassManifest](argList : List[AnyRef]) : T = {
		val mockWithArgs = getMockWithArgs[T]
		mockWithArgs.argList = argList
		mockWithArgs.mockObj
	}
	
	def validateArgs[A : ClassManifest](argList : List[Any], arg : A) : Boolean = {
		argList match {
			case head :: rest => arg == head
			case _ => false
		}
	}
	def validateArgs[A1 : ClassManifest, A2 : ClassManifest](argList : List[Any], arg1 : A1, arg2 : A2) : Boolean = {
		argList match {
			case head :: second :: rest => arg1 == head && arg2 == second
			case _ =>  false
		}
	}
}