package org.acme.scalautil

//trait Factory[+T] {
//	def create() : T = create(None)
//	def createWithId(id : String) : T = create(Some(Left(id)))
//	def createWithAnyRef(meta : AnyRef) : T = create(Some(Right(meta)))
//	
//	protected def create(meta : Option[Either[String, AnyRef]]) : T
//}

trait Factory {
	def create[T : ClassManifest] : T	
	def create[T : ClassManifest, A : ClassManifest](arg : A): T	
	def create[T : ClassManifest, A1 : ClassManifest, A2 : ClassManifest](arg1 : A1, arg2 : A2): T	

	def defineImpl[T : ClassManifest, I <: T : ClassManifest] : Unit
	
	def inject[T : ClassManifest] : T
	def register[T : ClassManifest](obj : T) : Unit
}

object Factory {
	def create[T](implicit f : Factory, cm : ClassManifest[T]) = f.create[T]
	def create[T, A](arg : A)(implicit f : Factory, cmt : ClassManifest[T], cma : ClassManifest[A]) = { f.create[T, A](arg) }
	def create[T, A1, A2](arg1 : A1, arg2 : A2)(implicit f : Factory, cmt : ClassManifest[T], cma1 : ClassManifest[A1], cma2 : ClassManifest[A2]) = { f.create[T, A1, A2](arg1, arg2) }

	def defineImpl[T, I <: T](implicit f : Factory, cmt : ClassManifest[T], cmi : ClassManifest[I]) = f.defineImpl[T, I]

	def inject[T](implicit f : Factory, cm : ClassManifest[T]) : T = f.inject[T]
	def register[T](obj : T)(implicit f : Factory, cm : ClassManifest[T]) : Unit = f.register(obj)

	implicit var implicitFactory : Factory = new FactoryDefault
}
