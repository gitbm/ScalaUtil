package org.acme.scalautil.tests

import org.acme.scalautil.Factory._

import org.junit.Assert._
import org.junit.After
import org.junit.AfterClass
import org.junit.Before
import org.junit.BeforeClass
import org.junit.Test

object constants {
	val NO_CTR = "TestImpl"
	val CTR_DEFAULT = "default"
	val CTR_VALUE = "FOO"
}

import constants._

trait TestTrait {
	def test : String
}

class TestImpl extends TestTrait {
	def test = NO_CTR
}

class TestImplCons(val cons : String) extends TestTrait {
	def this() { this(CTR_DEFAULT) }
	def test = cons
	
}

class TestFactory {

	@Test
	def testFactoryDefault() {
		defineImpl[TestTrait, TestImpl]
	
		val o = create[TestTrait]	
        assertEquals("class name", "org.acme.scalautil.tests.TestImpl", o.getClass.getName) 
		assertEquals("method", NO_CTR, o.test) 
	}

	@Test
	def testFactoryDefault2() {
		defineImpl[TestTrait, TestImplCons]

		val o = create[TestTrait]
		//println("o2 cl = " + o2.getClass.getName + ", " + o2.test) 
		assertEquals("class name", "org.acme.scalautil.tests.TestImplCons", o.getClass.getName) 
		assertEquals("method", CTR_DEFAULT, o.test) 	
	}

	
	@Test
	def testFactory1Default() {
		defineImpl[TestTrait, TestImplCons]
	
		val o = create[TestTrait, String](CTR_VALUE)
		assertEquals("class name", "org.acme.scalautil.tests.TestImplCons", o.getClass.getName) 
		assertEquals("method", CTR_VALUE, o.test) 

	}
}