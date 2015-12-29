package modules.test;

import modules.test.MappedExtension.MappedExtensionExtension;
import modules.test.domain.MappedExtension;

import org.junit.Assert;
import org.junit.Test;
import org.skyve.util.Util;
import org.skyve.wildcat.domain.messages.ReferentialConstraintViolationException;

public class PersistenceTests extends AbstractH2Test {
	@Test
	public void testPersistBizLock() throws Exception {
		MappedExtension test = Util.constructRandomInstance(u, m, med, 3);
		Assert.assertNull(test.getBizLock());
		Assert.assertNull(test.getAggregatedAssociation().getBizLock());
		Assert.assertNull(test.getAggregatedCollection().get(0).getBizLock());

		test = p.save(test);
		
		Assert.assertNotNull(test.getBizLock());
		Assert.assertNotNull(test.getAggregatedAssociation().getBizLock());
		Assert.assertNotNull(test.getAggregatedCollection().get(0).getBizLock());
	}
	
	@Test
	public void testPreAndPostSaveNoBase() throws Exception {
		MappedExtensionExtension test = Util.constructRandomInstance(u, m, med, 3);
		
		Assert.assertFalse(test.isPreSaveCalled());
		Assert.assertFalse(test.isPostSaveCalled());
		Assert.assertFalse(((MappedExtensionExtension) test.getAggregatedAssociation()).isPreSaveCalled());
		Assert.assertFalse(((MappedExtensionExtension) test.getAggregatedAssociation()).isPostSaveCalled());
		Assert.assertFalse(((MappedExtensionExtension) test.getAggregatedCollection().get(0)).isPreSaveCalled());
		Assert.assertFalse(((MappedExtensionExtension) test.getAggregatedCollection().get(0)).isPostSaveCalled());
		Assert.assertFalse(((MappedExtensionExtension) test.getAggregatedCollection().get(1)).isPreSaveCalled());
		Assert.assertFalse(((MappedExtensionExtension) test.getAggregatedCollection().get(1)).isPostSaveCalled());

		// Don't assign this coz we wanna test the old transient bean for evidence of bizlet calls
		MappedExtensionExtension persistedTest = p.save(test);
		
		Assert.assertTrue(test.isPreSaveCalled());
		Assert.assertTrue(((MappedExtensionExtension) test.getAggregatedAssociation()).isPreSaveCalled());
		Assert.assertTrue(((MappedExtensionExtension) test.getAggregatedCollection().get(0)).isPreSaveCalled());
		Assert.assertTrue(((MappedExtensionExtension) test.getAggregatedCollection().get(1)).isPreSaveCalled());

		// Post save is called on the now managed persistent version
		Assert.assertTrue(persistedTest.isPostSaveCalled());
		Assert.assertTrue(((MappedExtensionExtension) persistedTest.getAggregatedCollection().get(0)).isPostSaveCalled());
		Assert.assertTrue(((MappedExtensionExtension) persistedTest.getAggregatedAssociation()).isPostSaveCalled());
		Assert.assertTrue(((MappedExtensionExtension) persistedTest.getAggregatedCollection().get(1)).isPostSaveCalled());
	}
	
	@Test
	public void testDerivedPropertiesAvailableAfterSave() throws Exception {
		MappedExtensionExtension test = Util.constructRandomInstance(u, m, med, 4);
		
		Integer[] values = new Integer[] {test.getBaseDerivedInteger(),
											test.getDerivedInteger(),
											test.getAggregatedAssociation().getBaseDerivedInteger(),
											test.getAggregatedAssociation().getDerivedInteger(),
											test.getAggregatedCollection().get(0).getBaseDerivedInteger(),
											test.getAggregatedCollection().get(0).getDerivedInteger(),
											test.getAggregatedCollection().get(1).getBaseDerivedInteger(),
											test.getAggregatedCollection().get(1).getDerivedInteger()};
		for (Integer value : values) {
			Assert.assertNotNull(value);
		}
		
		test = p.save(test);
		
		Assert.assertEquals(values[0], test.getBaseDerivedInteger());
		Assert.assertEquals(values[1], test.getDerivedInteger());
		Assert.assertEquals(values[2], test.getAggregatedAssociation().getBaseDerivedInteger());
		Assert.assertEquals(values[3], test.getAggregatedAssociation().getDerivedInteger());
		Assert.assertEquals(values[4], test.getAggregatedCollection().get(0).getBaseDerivedInteger());
		Assert.assertEquals(values[5], test.getAggregatedCollection().get(0).getDerivedInteger());
		Assert.assertEquals(values[6], test.getAggregatedCollection().get(1).getBaseDerivedInteger());
		Assert.assertEquals(values[7], test.getAggregatedCollection().get(1).getDerivedInteger());
	}
	
	@Test
	public void testComposedCascadeDelete() throws Exception {
		MappedExtension test = Util.constructRandomInstance(u, m, med, 2);
		test.setAggregatedAssociation(null);
		test.getAggregatedCollection().clear();
		test = p.save(test);
		
		Assert.assertEquals(4, p.newSQL("select count(1) from TEST_MappedExtension").scalarResult(Number.class).intValue()); 
		Assert.assertEquals(2, p.newSQL("select count(1) from TEST_MappedExtension_composedCollection").scalarResult(Number.class).intValue()); 
		
		p.delete(test);

		// There should be no TEST_MappedExtension rows in the database since they cascade
		Assert.assertEquals(0, p.newSQL("select count(1) from TEST_MappedExtension").scalarResult(Number.class).intValue()); 
		Assert.assertEquals(0, p.newSQL("select count(1) from TEST_MappedExtension_composedCollection").scalarResult(Number.class).intValue()); 
	}
	
	@Test
	public void testAggregatedCascadeDelete() throws Exception {
		MappedExtension test = Util.constructRandomInstance(u, m, med, 2);
		test = p.save(test);
		
		Assert.assertEquals(7, p.newSQL("select count(1) from TEST_MappedExtension").scalarResult(Number.class).intValue()); 
		Assert.assertEquals(2, p.newSQL("select count(1) from TEST_MappedExtension_composedCollection").scalarResult(Number.class).intValue()); 
		Assert.assertEquals(2, p.newSQL("select count(1) from TEST_MappedExtension_aggregatedCollection").scalarResult(Number.class).intValue()); 
		
		p.delete(test);

		// There should be 3 TEST_MappedExtension rows in the database since they cascade
		// but the aggregated ones (2 in collections and 1 associations) are left behind
		Assert.assertEquals(3, p.newSQL("select count(1) from TEST_MappedExtension").scalarResult(Number.class).intValue()); 
		Assert.assertEquals(0, p.newSQL("select count(1) from TEST_MappedExtension_composedCollection").scalarResult(Number.class).intValue()); 
		Assert.assertEquals(0, p.newSQL("select count(1) from TEST_MappedExtension_aggregatedCollection").scalarResult(Number.class).intValue()); 
	}

	@Test(expected = ReferentialConstraintViolationException.class)
	public void testAggregatedAssociationReferentialIntegrity() throws Exception {
		MappedExtension test = Util.constructRandomInstance(u, m, med, 2);
		test = p.save(test);
		
		p.delete(test.getAggregatedAssociation());
	}

	@Test(expected = ReferentialConstraintViolationException.class)
	public void testAggregatedCollectionReferentialIntegrity() throws Exception {
		MappedExtension test = Util.constructRandomInstance(u, m, med, 2);
		test = p.save(test);
		
		p.delete(test.getAggregatedCollection().get(0));
	}

	@Test(expected = ReferentialConstraintViolationException.class)
	public void testComposedAssociationReferentialIntegrity() throws Exception {
		MappedExtension test = Util.constructRandomInstance(u, m, med, 2);
		test = p.save(test);
		
		p.delete(test.getComposedAssociation());
	}

	@Test(expected = ReferentialConstraintViolationException.class)
	public void testComposedCollectionReferentialIntegrity() throws Exception {
		MappedExtension test = Util.constructRandomInstance(u, m, med, 2);
		test = p.save(test);
		
		p.delete(test.getComposedCollection().get(0));
	}
}