package modules.test;

import org.junit.Assert;
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

public class GenerationTests extends AbstractSkyveTest {

	/**
	 * Test the generated domain for the various extension hierarchies modelled in the test module.
	 */
	@Test
	@SuppressWarnings({ "cast", "static-method" })
	public void testGenerateExtensionHierarchy() {
		MappedBase mb = new MappedBase();
		MappedExtensionSingleStrategy mess = MappedExtensionSingleStrategy.newInstance();
		MappedExtensionJoinedStrategy mejs = MappedExtensionJoinedStrategy.newInstance();
		MappedSubclassedSingleStrategy msss = new MappedSubclassedSingleStrategy();
		MappedSubclassedJoinedStrategy msjs = new MappedSubclassedJoinedStrategy();
		Assert.assertTrue(new AllAttributesPersistent() instanceof PersistentBean);
		Assert.assertTrue(new Hierarchical() instanceof HierarchicalBean<?>);
		Assert.assertTrue(mb instanceof PersistentBean);
		Assert.assertFalse(MappedBase.class.isAnnotationPresent(PolymorphicPersistentBean.class)); // mapped bean
		Assert.assertTrue(mejs instanceof MappedBase);
		Assert.assertTrue(MappedExtensionJoinedStrategy.class.isAnnotationPresent(PolymorphicPersistentBean.class)); // joined strategy
		Assert.assertTrue(mess instanceof MappedBase);
		Assert.assertTrue(MappedExtensionSingleStrategy.class.isAnnotationPresent(PolymorphicPersistentBean.class)); // single strategy
		Assert.assertTrue(msjs instanceof MappedExtensionJoinedStrategyExtension);
		Assert.assertFalse(MappedExtensionJoinedStrategyExtension.class.isAnnotationPresent(PolymorphicPersistentBean.class)); // no subclasses
		Assert.assertTrue(msss instanceof MappedExtensionSingleStrategyExtension);
		Assert.assertFalse(MappedExtensionSingleStrategyExtension.class.isAnnotationPresent(PolymorphicPersistentBean.class)); // no subclasses
	}
	
	/**
	 * Test null assignment from the association side of a one to one inverse relationship.
	 */
	@Test
	@SuppressWarnings("static-method")
	public void testNullOneToOne() {
		InverseOneToOnePersistent one = InverseOneToOnePersistent.newInstance();
		Assert.assertNull(one.getAggAssociation());
		Assert.assertNull(one.getInvAggAssociation());
		
		InverseOneToOnePersistent two = InverseOneToOnePersistent.newInstance();
		one.setAggAssociation(two);
		Assert.assertEquals(two, one.getAggAssociation());
		Assert.assertEquals(one, two.getInvAggAssociation());

		one.setAggAssociation(null);
		Assert.assertNull(one.getAggAssociation());
		Assert.assertNull(two.getInvAggAssociation());
	}
	
	/**
	 * Test double assignment from the association side of a one to one inverse relationship.
	 */
	@Test
	@SuppressWarnings("static-method")
	public void testDoubleAssignOneToOne() {
		InverseOneToOnePersistent one = InverseOneToOnePersistent.newInstance();
		Assert.assertNull(one.getAggAssociation());
		Assert.assertNull(one.getInvAggAssociation());
		
		InverseOneToOnePersistent two = InverseOneToOnePersistent.newInstance();
		one.setAggAssociation(two);
		Assert.assertEquals(two, one.getAggAssociation());
		Assert.assertEquals(one, two.getInvAggAssociation());

		InverseOneToOnePersistent three = InverseOneToOnePersistent.newInstance();
		one.setAggAssociation(three);
		Assert.assertEquals(three, one.getAggAssociation());
		Assert.assertEquals(one, three.getInvAggAssociation());
		Assert.assertNull(two.getAggAssociation());
		Assert.assertNull(two.getInvAggAssociation());
	}

	/**
	 * Test null assignment from the inverse side of a one to one inverse relationship.
	 */
	@Test
	@SuppressWarnings("static-method")
	public void testNullInverseOneToOne() {
		InverseOneToOnePersistent one = InverseOneToOnePersistent.newInstance();
		Assert.assertNull(one.getAggAssociation());
		Assert.assertNull(one.getInvAggAssociation());
		
		InverseOneToOnePersistent two = InverseOneToOnePersistent.newInstance();
		one.setInvAggAssociation(two);
		Assert.assertEquals(two, one.getInvAggAssociation());
		Assert.assertEquals(one, two.getAggAssociation());

		one.setInvAggAssociation(null);
		Assert.assertNull(one.getInvAggAssociation());
		Assert.assertNull(two.getAggAssociation());
	}

	/**
	 * Test double assignment from the association side of a one to one inverse relationship.
	 */
	@Test
	@SuppressWarnings("static-method")
	public void testDoubleAssignInverseOneToOne() {
		InverseOneToOnePersistent one = InverseOneToOnePersistent.newInstance();
		Assert.assertNull(one.getAggAssociation());
		Assert.assertNull(one.getInvAggAssociation());
		
		InverseOneToOnePersistent two = InverseOneToOnePersistent.newInstance();
		one.setInvAggAssociation(two);
		Assert.assertEquals(two, one.getInvAggAssociation());
		Assert.assertEquals(one, two.getAggAssociation());

		InverseOneToOnePersistent three = InverseOneToOnePersistent.newInstance();
		one.setInvAggAssociation(three);
		Assert.assertEquals(three, one.getInvAggAssociation());
		Assert.assertEquals(one, three.getAggAssociation());
		Assert.assertNull(two.getAggAssociation());
		Assert.assertNull(two.getInvAggAssociation());
	}

	/**
	 * Test null assignment from the association side of a one to many inverse relationship.
	 */
	@Test
	@SuppressWarnings("static-method")
	public void testNullOneToMany() {
		InverseOneToManyPersistent one = InverseOneToManyPersistent.newInstance();
		Assert.assertNull(one.getAggAssociation());
		Assert.assertTrue(one.getInvAggAssociation().isEmpty());
		
		InverseOneToManyPersistent two = InverseOneToManyPersistent.newInstance();
		one.setAggAssociation(two);
		Assert.assertEquals(two, one.getAggAssociation());
		Assert.assertTrue(two.getInvAggAssociation().contains(one));

		one.setAggAssociation(null);
		Assert.assertNull(one.getAggAssociation());
		Assert.assertTrue(two.getInvAggAssociation().isEmpty());
	}
	
	/**
	 * Test double assignment, repeated assignment and null assignment
	 * from the association side of a one to many inverse relationship.
	 */
	@Test
	@SuppressWarnings("static-method")
	public void testDoubleDualAndNullAssignOneToMany() {
		InverseOneToManyPersistent one = InverseOneToManyPersistent.newInstance();
		Assert.assertNull(one.getAggAssociation());
		Assert.assertTrue(one.getInvAggAssociation().isEmpty());
		
		InverseOneToManyPersistent two = InverseOneToManyPersistent.newInstance();
		one.setAggAssociation(two);
		Assert.assertEquals(two, one.getAggAssociation());
		Assert.assertTrue(two.getInvAggAssociation().contains(one));

		InverseOneToManyPersistent three = InverseOneToManyPersistent.newInstance();
		one.setAggAssociation(three);
		Assert.assertEquals(three, one.getAggAssociation());
		Assert.assertTrue(three.getInvAggAssociation().contains(one));
		Assert.assertNull(two.getAggAssociation());
		Assert.assertTrue(two.getInvAggAssociation().isEmpty());
		
		one.setAggAssociation(three);
		Assert.assertEquals(three, one.getAggAssociation());
		Assert.assertEquals(three.getInvAggAssociation().indexOf(one), three.getInvAggAssociation().lastIndexOf(one));
		Assert.assertNull(two.getAggAssociation());
		Assert.assertTrue(two.getInvAggAssociation().isEmpty());

		one.setAggAssociation(null);
		Assert.assertNull(one.getAggAssociation());
		Assert.assertTrue(one.getInvAggAssociation().isEmpty());
	}

	/**
	 * Test double add, repeated add and remove
	 * from the inverse side of a one to many inverse relationship.
	 */
	@Test
	@SuppressWarnings("static-method")
	public void testDoubleAndDualAddAndRemoveInverseOneToMany() {
		InverseOneToManyPersistent one = InverseOneToManyPersistent.newInstance();
		Assert.assertNull(one.getAggAssociation());
		Assert.assertTrue(one.getInvAggAssociation().isEmpty());
		
		InverseOneToManyPersistent two = InverseOneToManyPersistent.newInstance();
		one.addInvAggAssociationElement(two);
		Assert.assertTrue(one.getInvAggAssociation().contains(two));
		Assert.assertEquals(one, two.getAggAssociation());

		InverseOneToManyPersistent three = InverseOneToManyPersistent.newInstance();
		one.addInvAggAssociationElement(three);
		Assert.assertTrue(one.getInvAggAssociation().contains(three));
		Assert.assertEquals(one, three.getAggAssociation());
		Assert.assertEquals(one, two.getAggAssociation());
		Assert.assertTrue(one.getInvAggAssociation().contains(two));
		
		one.addInvAggAssociationElement(three);
		Assert.assertEquals(one.getInvAggAssociation().indexOf(three), one.getInvAggAssociation().lastIndexOf(three));
		Assert.assertEquals(one, three.getAggAssociation());
		Assert.assertTrue(one.getInvAggAssociation().contains(two));
		Assert.assertEquals(one, two.getAggAssociation());
		
		one.removeInvAggAssociationElement(two);
		Assert.assertFalse(one.getInvAggAssociation().contains(two));
		Assert.assertNull(two.getAggAssociation());
	}
	
	/**
	 * Test double add, repeated add and remove
	 * from the collection side of a one to many inverse relationship.
	 */
	@Test
	@SuppressWarnings("static-method")
	public void testDoubleAndDualAddAndRemoveManyToMany() {
		InverseManyToManyPersistent one = InverseManyToManyPersistent.newInstance();
		Assert.assertTrue(one.getAggCollection().isEmpty());
		Assert.assertTrue(one.getInvAggCollection().isEmpty());
		
		InverseManyToManyPersistent two = InverseManyToManyPersistent.newInstance();
		one.addAggCollectionElement(two);
		Assert.assertTrue(one.getAggCollection().contains(two));
		Assert.assertTrue(two.getInvAggCollection().contains(one));

		InverseManyToManyPersistent three = InverseManyToManyPersistent.newInstance();
		one.addAggCollectionElement(three);
		Assert.assertTrue(one.getAggCollection().contains(three));
		Assert.assertTrue(three.getInvAggCollection().contains(one));
		Assert.assertTrue(two.getInvAggCollection().contains(one));
		Assert.assertTrue(one.getAggCollection().contains(two));
		
		one.addAggCollectionElement(three);
		Assert.assertNotEquals(one.getAggCollection().indexOf(three), one.getAggCollection().lastIndexOf(three));
		Assert.assertTrue(three.getInvAggCollection().contains(one));
		Assert.assertTrue(one.getAggCollection().contains(two));
		Assert.assertTrue(two.getInvAggCollection().contains(one));
		
		one.removeAggCollectionElement(two);
		Assert.assertFalse(one.getAggCollection().contains(two));
		Assert.assertTrue(two.getInvAggCollection().isEmpty());
		Assert.assertEquals(2, one.getAggCollection().size());
		Assert.assertEquals(2, three.getInvAggCollection().size());
		
		one.removeAggCollectionElement(three);
		Assert.assertTrue(one.getAggCollection().contains(three));
		Assert.assertTrue(three.getInvAggCollection().contains(one));
		Assert.assertEquals(1, one.getAggCollection().size());
		Assert.assertEquals(1, three.getInvAggCollection().size());
	}

	/**
	 * Test double add, repeated add and remove
	 * from the inverse side of a one to many inverse relationship.
	 */
	@Test
	@SuppressWarnings("static-method")
	public void testDoubleAndDualAddAndRemoveInverseManyToMany() {
		InverseManyToManyPersistent one = InverseManyToManyPersistent.newInstance();
		Assert.assertTrue(one.getAggCollection().isEmpty());
		Assert.assertTrue(one.getInvAggCollection().isEmpty());
		
		InverseManyToManyPersistent two = InverseManyToManyPersistent.newInstance();
		one.addInvAggCollectionElement(two);
		Assert.assertTrue(one.getInvAggCollection().contains(two));
		Assert.assertTrue(two.getAggCollection().contains(one));

		InverseManyToManyPersistent three = InverseManyToManyPersistent.newInstance();
		one.addInvAggCollectionElement(three);
		Assert.assertTrue(one.getInvAggCollection().contains(three));
		Assert.assertTrue(three.getAggCollection().contains(one));
		Assert.assertTrue(two.getAggCollection().contains(one));
		Assert.assertTrue(one.getInvAggCollection().contains(two));
		
		one.addInvAggCollectionElement(three);
		Assert.assertNotEquals(one.getInvAggCollection().indexOf(three), one.getInvAggCollection().lastIndexOf(three));
		Assert.assertTrue(three.getAggCollection().contains(one));
		Assert.assertTrue(one.getInvAggCollection().contains(two));
		Assert.assertTrue(two.getAggCollection().contains(one));
		
		one.removeInvAggCollectionElement(two);
		Assert.assertFalse(one.getInvAggCollection().contains(two));
		Assert.assertTrue(two.getAggCollection().isEmpty());
		Assert.assertEquals(2, one.getInvAggCollection().size());
		Assert.assertEquals(2, three.getAggCollection().size());
		
		one.removeInvAggCollectionElement(three);
		Assert.assertTrue(one.getInvAggCollection().contains(three));
		Assert.assertTrue(three.getAggCollection().contains(one));
		Assert.assertEquals(1, one.getInvAggCollection().size());
		Assert.assertEquals(1, three.getAggCollection().size());
	}
}
