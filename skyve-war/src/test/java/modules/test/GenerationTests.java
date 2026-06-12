package modules.test;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.skyve.domain.HierarchicalBean;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.PolymorphicPersistentBean;

import modules.test.MappedExtensionJoinedStrategy.MappedExtensionJoinedStrategyExtension;
import modules.test.MappedExtensionSingleStrategy.MappedExtensionSingleStrategyExtension;
import modules.test.domain.AllAttributesPersistent;
import modules.test.domain.Hierarchical;
import modules.test.domain.InverseManyToManyPersistent;
import modules.test.domain.InverseOneToManyPersistent;
import modules.test.domain.InverseOneToOnePersistent;
import modules.test.domain.MappedBase;
import modules.test.domain.MappedExtensionJoinedStrategy;
import modules.test.domain.MappedExtensionSingleStrategy;
import modules.test.domain.MappedSubclassedJoinedStrategy;
import modules.test.domain.MappedSubclassedSingleStrategy;

class GenerationTests extends AbstractSkyveTest {

	/**
	 * Test the generated domain for the various extension hierarchies modelled in the test module.
	 */
	@Test
	@SuppressWarnings({ "cast", "static-method" })
	void testGenerateExtensionHierarchy() {
		MappedBase mb = new MappedBase();
		MappedExtensionSingleStrategy mess = MappedExtensionSingleStrategy.newInstance();
		MappedExtensionJoinedStrategy mejs = MappedExtensionJoinedStrategy.newInstance();
		MappedSubclassedSingleStrategy msss = new MappedSubclassedSingleStrategy();
		MappedSubclassedJoinedStrategy msjs = new MappedSubclassedJoinedStrategy();
		Assertions.assertTrue(new AllAttributesPersistent() instanceof PersistentBean);
		Assertions.assertTrue(new Hierarchical() instanceof HierarchicalBean<?>);
		Assertions.assertTrue(mb instanceof PersistentBean);
		Assertions.assertFalse(MappedBase.class.isAnnotationPresent(PolymorphicPersistentBean.class)); // mapped bean
		Assertions.assertTrue(mejs instanceof MappedBase);
		Assertions.assertTrue(MappedExtensionJoinedStrategy.class.isAnnotationPresent(PolymorphicPersistentBean.class)); // joined strategy
		Assertions.assertTrue(mess instanceof MappedBase);
		Assertions.assertTrue(MappedExtensionSingleStrategy.class.isAnnotationPresent(PolymorphicPersistentBean.class)); // single strategy
		Assertions.assertTrue(msjs instanceof MappedExtensionJoinedStrategyExtension);
		Assertions.assertFalse(MappedExtensionJoinedStrategyExtension.class.isAnnotationPresent(PolymorphicPersistentBean.class)); // no subclasses
		Assertions.assertTrue(msss instanceof MappedExtensionSingleStrategyExtension);
		Assertions.assertFalse(MappedExtensionSingleStrategyExtension.class.isAnnotationPresent(PolymorphicPersistentBean.class)); // no subclasses
	}
	
	/**
	 * Test null assignment from the association side of a one to one inverse relationship.
	 */
	@Test
	@SuppressWarnings("static-method")
	void testNullOneToOne() {
		InverseOneToOnePersistent one = InverseOneToOnePersistent.newInstance();
		Assertions.assertNull(one.getAggAssociation());
		Assertions.assertNull(one.getInvAggAssociation());
		
		InverseOneToOnePersistent two = InverseOneToOnePersistent.newInstance();
		one.setAggAssociation(two);
		Assertions.assertEquals(two, one.getAggAssociation());
		Assertions.assertEquals(one, two.getInvAggAssociation());

		one.setAggAssociation(null);
		Assertions.assertNull(one.getAggAssociation());
		Assertions.assertNull(two.getInvAggAssociation());
	}
	
	/**
	 * Test double assignment from the association side of a one to one inverse relationship.
	 */
	@Test
	@SuppressWarnings("static-method")
	void testDoubleAssignOneToOne() {
		InverseOneToOnePersistent one = InverseOneToOnePersistent.newInstance();
		Assertions.assertNull(one.getAggAssociation());
		Assertions.assertNull(one.getInvAggAssociation());
		
		InverseOneToOnePersistent two = InverseOneToOnePersistent.newInstance();
		one.setAggAssociation(two);
		Assertions.assertEquals(two, one.getAggAssociation());
		Assertions.assertEquals(one, two.getInvAggAssociation());

		InverseOneToOnePersistent three = InverseOneToOnePersistent.newInstance();
		one.setAggAssociation(three);
		Assertions.assertEquals(three, one.getAggAssociation());
		Assertions.assertEquals(one, three.getInvAggAssociation());
		Assertions.assertNull(two.getAggAssociation());
		Assertions.assertNull(two.getInvAggAssociation());
	}

	/**
	 * Test null assignment from the inverse side of a one to one inverse relationship.
	 */
	@Test
	@SuppressWarnings("static-method")
	void testNullInverseOneToOne() {
		InverseOneToOnePersistent one = InverseOneToOnePersistent.newInstance();
		Assertions.assertNull(one.getAggAssociation());
		Assertions.assertNull(one.getInvAggAssociation());
		
		InverseOneToOnePersistent two = InverseOneToOnePersistent.newInstance();
		one.setInvAggAssociation(two);
		Assertions.assertEquals(two, one.getInvAggAssociation());
		Assertions.assertEquals(one, two.getAggAssociation());

		one.setInvAggAssociation(null);
		Assertions.assertNull(one.getInvAggAssociation());
		Assertions.assertNull(two.getAggAssociation());
	}

	/**
	 * Test double assignment from the association side of a one to one inverse relationship.
	 */
	@Test
	@SuppressWarnings("static-method")
	void testDoubleAssignInverseOneToOne() {
		InverseOneToOnePersistent one = InverseOneToOnePersistent.newInstance();
		Assertions.assertNull(one.getAggAssociation());
		Assertions.assertNull(one.getInvAggAssociation());
		
		InverseOneToOnePersistent two = InverseOneToOnePersistent.newInstance();
		one.setInvAggAssociation(two);
		Assertions.assertEquals(two, one.getInvAggAssociation());
		Assertions.assertEquals(one, two.getAggAssociation());

		InverseOneToOnePersistent three = InverseOneToOnePersistent.newInstance();
		one.setInvAggAssociation(three);
		Assertions.assertEquals(three, one.getInvAggAssociation());
		Assertions.assertEquals(one, three.getAggAssociation());
		Assertions.assertNull(two.getAggAssociation());
		Assertions.assertNull(two.getInvAggAssociation());
	}

	/**
	 * Test null assignment from the association side of a one to many inverse relationship.
	 */
	@Test
	@SuppressWarnings("static-method")
	void testNullOneToMany() {
		InverseOneToManyPersistent one = InverseOneToManyPersistent.newInstance();
		Assertions.assertNull(one.getAggAssociation());
		Assertions.assertTrue(one.getInvAggAssociation().isEmpty());
		
		InverseOneToManyPersistent two = InverseOneToManyPersistent.newInstance();
		one.setAggAssociation(two);
		Assertions.assertEquals(two, one.getAggAssociation());
		Assertions.assertTrue(two.getInvAggAssociation().contains(one));

		one.setAggAssociation(null);
		Assertions.assertNull(one.getAggAssociation());
		Assertions.assertTrue(two.getInvAggAssociation().isEmpty());
	}
	
	/**
	 * Test double assignment, repeated assignment and null assignment
	 * from the association side of a one to many inverse relationship.
	 */
	@Test
	@SuppressWarnings("static-method")
	void testDoubleDualAndNullAssignOneToMany() {
		InverseOneToManyPersistent one = InverseOneToManyPersistent.newInstance();
		Assertions.assertNull(one.getAggAssociation());
		Assertions.assertTrue(one.getInvAggAssociation().isEmpty());
		
		InverseOneToManyPersistent two = InverseOneToManyPersistent.newInstance();
		one.setAggAssociation(two);
		Assertions.assertEquals(two, one.getAggAssociation());
		Assertions.assertTrue(two.getInvAggAssociation().contains(one));

		InverseOneToManyPersistent three = InverseOneToManyPersistent.newInstance();
		one.setAggAssociation(three);
		Assertions.assertEquals(three, one.getAggAssociation());
		Assertions.assertTrue(three.getInvAggAssociation().contains(one));
		Assertions.assertNull(two.getAggAssociation());
		Assertions.assertTrue(two.getInvAggAssociation().isEmpty());
		
		one.setAggAssociation(three);
		Assertions.assertEquals(three, one.getAggAssociation());
		Assertions.assertEquals(three.getInvAggAssociation().indexOf(one), three.getInvAggAssociation().lastIndexOf(one));
		Assertions.assertNull(two.getAggAssociation());
		Assertions.assertTrue(two.getInvAggAssociation().isEmpty());

		one.setAggAssociation(null);
		Assertions.assertNull(one.getAggAssociation());
		Assertions.assertTrue(one.getInvAggAssociation().isEmpty());
	}

	/**
	 * Test double add, repeated add and remove
	 * from the inverse side of a one to many inverse relationship.
	 */
	@Test
	@SuppressWarnings("static-method")
	void testDoubleAndDualAddAndRemoveInverseOneToMany() {
		InverseOneToManyPersistent one = InverseOneToManyPersistent.newInstance();
		Assertions.assertNull(one.getAggAssociation());
		Assertions.assertTrue(one.getInvAggAssociation().isEmpty());
		
		InverseOneToManyPersistent two = InverseOneToManyPersistent.newInstance();
		one.addInvAggAssociationElement(two);
		Assertions.assertTrue(one.getInvAggAssociation().contains(two));
		Assertions.assertEquals(one, two.getAggAssociation());

		InverseOneToManyPersistent three = InverseOneToManyPersistent.newInstance();
		one.addInvAggAssociationElement(three);
		Assertions.assertTrue(one.getInvAggAssociation().contains(three));
		Assertions.assertEquals(one, three.getAggAssociation());
		Assertions.assertEquals(one, two.getAggAssociation());
		Assertions.assertTrue(one.getInvAggAssociation().contains(two));
		
		one.addInvAggAssociationElement(three);
		Assertions.assertEquals(one.getInvAggAssociation().indexOf(three), one.getInvAggAssociation().lastIndexOf(three));
		Assertions.assertEquals(one, three.getAggAssociation());
		Assertions.assertTrue(one.getInvAggAssociation().contains(two));
		Assertions.assertEquals(one, two.getAggAssociation());
		
		one.removeInvAggAssociationElement(two);
		Assertions.assertFalse(one.getInvAggAssociation().contains(two));
		Assertions.assertNull(two.getAggAssociation());
	}
	
	/**
	 * Test double add, repeated add and remove
	 * from the collection side of a one to many inverse relationship.
	 */
	@Test
	@SuppressWarnings("static-method")
	void testDoubleAndDualAddAndRemoveManyToMany() {
		InverseManyToManyPersistent one = InverseManyToManyPersistent.newInstance();
		Assertions.assertTrue(one.getAggCollection().isEmpty());
		Assertions.assertTrue(one.getInvAggCollection().isEmpty());
		
		InverseManyToManyPersistent two = InverseManyToManyPersistent.newInstance();
		one.addAggCollectionElement(two);
		Assertions.assertTrue(one.getAggCollection().contains(two));
		Assertions.assertTrue(two.getInvAggCollection().contains(one));

		InverseManyToManyPersistent three = InverseManyToManyPersistent.newInstance();
		one.addAggCollectionElement(three);
		Assertions.assertTrue(one.getAggCollection().contains(three));
		Assertions.assertTrue(three.getInvAggCollection().contains(one));
		Assertions.assertTrue(two.getInvAggCollection().contains(one));
		Assertions.assertTrue(one.getAggCollection().contains(two));
		
		one.addAggCollectionElement(three);
		Assertions.assertNotEquals(one.getAggCollection().indexOf(three), one.getAggCollection().lastIndexOf(three));
		Assertions.assertTrue(three.getInvAggCollection().contains(one));
		Assertions.assertTrue(one.getAggCollection().contains(two));
		Assertions.assertTrue(two.getInvAggCollection().contains(one));
		
		one.removeAggCollectionElement(two);
		Assertions.assertFalse(one.getAggCollection().contains(two));
		Assertions.assertTrue(two.getInvAggCollection().isEmpty());
		Assertions.assertEquals(2, one.getAggCollection().size());
		Assertions.assertEquals(2, three.getInvAggCollection().size());
		
		one.removeAggCollectionElement(three);
		Assertions.assertTrue(one.getAggCollection().contains(three));
		Assertions.assertTrue(three.getInvAggCollection().contains(one));
		Assertions.assertEquals(1, one.getAggCollection().size());
		Assertions.assertEquals(1, three.getInvAggCollection().size());
	}

	/**
	 * Test double add, repeated add and remove
	 * from the inverse side of a one to many inverse relationship.
	 */
	@Test
	@SuppressWarnings("static-method")
	void testDoubleAndDualAddAndRemoveInverseManyToMany() {
		InverseManyToManyPersistent one = InverseManyToManyPersistent.newInstance();
		Assertions.assertTrue(one.getAggCollection().isEmpty());
		Assertions.assertTrue(one.getInvAggCollection().isEmpty());
		
		InverseManyToManyPersistent two = InverseManyToManyPersistent.newInstance();
		one.addInvAggCollectionElement(two);
		Assertions.assertTrue(one.getInvAggCollection().contains(two));
		Assertions.assertTrue(two.getAggCollection().contains(one));

		InverseManyToManyPersistent three = InverseManyToManyPersistent.newInstance();
		one.addInvAggCollectionElement(three);
		Assertions.assertTrue(one.getInvAggCollection().contains(three));
		Assertions.assertTrue(three.getAggCollection().contains(one));
		Assertions.assertTrue(two.getAggCollection().contains(one));
		Assertions.assertTrue(one.getInvAggCollection().contains(two));
		
		one.addInvAggCollectionElement(three);
		Assertions.assertNotEquals(one.getInvAggCollection().indexOf(three), one.getInvAggCollection().lastIndexOf(three));
		Assertions.assertTrue(three.getAggCollection().contains(one));
		Assertions.assertTrue(one.getInvAggCollection().contains(two));
		Assertions.assertTrue(two.getAggCollection().contains(one));
		
		one.removeInvAggCollectionElement(two);
		Assertions.assertFalse(one.getInvAggCollection().contains(two));
		Assertions.assertTrue(two.getAggCollection().isEmpty());
		Assertions.assertEquals(2, one.getInvAggCollection().size());
		Assertions.assertEquals(2, three.getAggCollection().size());
		
		one.removeInvAggCollectionElement(three);
		Assertions.assertTrue(one.getInvAggCollection().contains(three));
		Assertions.assertTrue(three.getAggCollection().contains(one));
		Assertions.assertEquals(1, one.getInvAggCollection().size());
		Assertions.assertEquals(1, three.getAggCollection().size());
	}
}
