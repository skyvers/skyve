package modules.test;

import modules.test.MappedExtensionJoinedStrategy.MappedExtensionJoinedStrategyExtension;
import modules.test.MappedExtensionSingleStrategy.MappedExtensionSingleStrategyExtension;
import modules.test.domain.MappedExtensionJoinedStrategy;
import modules.test.domain.MappedExtensionSingleStrategy;
import modules.test.domain.MappedSubclassedJoinedStrategy;
import modules.test.domain.MappedSubclassedSingleStrategy;

import org.junit.Assert;
import org.junit.Test;
import org.skyve.util.Util;
import org.skyve.wildcat.domain.messages.ReferentialConstraintViolationException;

public class PersistenceTests extends AbstractH2Test {
	@Test
	public void testPersistBizLockEJS() throws Exception {
		MappedExtensionJoinedStrategy test = Util.constructRandomInstance(u, m, mejsd, 3);
		Assert.assertNull(test.getBizLock());
		Assert.assertNull(test.getAggregatedAssociation().getBizLock());
		Assert.assertNull(test.getAggregatedCollection().get(0).getBizLock());

		test = p.save(test);
		
		Assert.assertNotNull(test.getBizLock());
		Assert.assertNotNull(test.getAggregatedAssociation().getBizLock());
		Assert.assertNotNull(test.getAggregatedCollection().get(0).getBizLock());
	}

	@Test
	public void testPersistBizLockESS() throws Exception {
		MappedExtensionSingleStrategy test = Util.constructRandomInstance(u, m, messd, 3);
		Assert.assertNull(test.getBizLock());
		Assert.assertNull(test.getAggregatedAssociation().getBizLock());
		Assert.assertNull(test.getAggregatedCollection().get(0).getBizLock());

		test = p.save(test);
		
		Assert.assertNotNull(test.getBizLock());
		Assert.assertNotNull(test.getAggregatedAssociation().getBizLock());
		Assert.assertNotNull(test.getAggregatedCollection().get(0).getBizLock());
	}
	
	@Test
	public void testPersistBizLockSJS() throws Exception {
		MappedSubclassedJoinedStrategy test = Util.constructRandomInstance(u, m, msjsd, 3);
		Assert.assertNull(test.getBizLock());
		Assert.assertNull(test.getAggregatedAssociation().getBizLock());
		Assert.assertNull(test.getAggregatedCollection().get(0).getBizLock());

		test = p.save(test);
		
		Assert.assertNotNull(test.getBizLock());
		Assert.assertNotNull(test.getAggregatedAssociation().getBizLock());
		Assert.assertNotNull(test.getAggregatedCollection().get(0).getBizLock());
	}

	@Test
	public void testPersistBizLockSSS() throws Exception {
		MappedSubclassedSingleStrategy test = Util.constructRandomInstance(u, m, msssd, 3);
		Assert.assertNull(test.getBizLock());
		Assert.assertNull(test.getAggregatedAssociation().getBizLock());
		Assert.assertNull(test.getAggregatedCollection().get(0).getBizLock());

		test = p.save(test);
		
		Assert.assertNotNull(test.getBizLock());
		Assert.assertNotNull(test.getAggregatedAssociation().getBizLock());
		Assert.assertNotNull(test.getAggregatedCollection().get(0).getBizLock());
	}

	@Test
	public void testPreAndPostSaveNoBaseESS() throws Exception {
		MappedExtensionSingleStrategyExtension test = Util.constructRandomInstance(u, m, messd, 3);
		
		Assert.assertFalse(test.isPreSaveCalled());
		Assert.assertFalse(test.isPostSaveCalled());
		Assert.assertFalse(((MappedExtensionSingleStrategyExtension) test.getAggregatedAssociation()).isPreSaveCalled());
		Assert.assertFalse(((MappedExtensionSingleStrategyExtension) test.getAggregatedAssociation()).isPostSaveCalled());
		Assert.assertFalse(((MappedExtensionSingleStrategyExtension) test.getAggregatedCollection().get(0)).isPreSaveCalled());
		Assert.assertFalse(((MappedExtensionSingleStrategyExtension) test.getAggregatedCollection().get(0)).isPostSaveCalled());
		Assert.assertFalse(((MappedExtensionSingleStrategyExtension) test.getAggregatedCollection().get(1)).isPreSaveCalled());
		Assert.assertFalse(((MappedExtensionSingleStrategyExtension) test.getAggregatedCollection().get(1)).isPostSaveCalled());

		// Don't assign this coz we wanna test the old transient bean for evidence of bizlet calls
		MappedExtensionSingleStrategyExtension persistedTest = p.save(test);
		
		Assert.assertTrue(test.isPreSaveCalled());
		Assert.assertTrue(((MappedExtensionSingleStrategyExtension) test.getAggregatedAssociation()).isPreSaveCalled());
		Assert.assertTrue(((MappedExtensionSingleStrategyExtension) test.getAggregatedCollection().get(0)).isPreSaveCalled());
		Assert.assertTrue(((MappedExtensionSingleStrategyExtension) test.getAggregatedCollection().get(1)).isPreSaveCalled());

		// Post save is called on the now managed persistent version
		Assert.assertTrue(persistedTest.isPostSaveCalled());
		Assert.assertTrue(((MappedExtensionSingleStrategyExtension) persistedTest.getAggregatedCollection().get(0)).isPostSaveCalled());
		Assert.assertTrue(((MappedExtensionSingleStrategyExtension) persistedTest.getAggregatedAssociation()).isPostSaveCalled());
		Assert.assertTrue(((MappedExtensionSingleStrategyExtension) persistedTest.getAggregatedCollection().get(1)).isPostSaveCalled());
	}
	
	@Test
	public void testPreAndPostSaveNoBaseEJS() throws Exception {
		MappedExtensionJoinedStrategyExtension test = Util.constructRandomInstance(u, m, mejsd, 3);
		
		Assert.assertFalse(test.isPreSaveCalled());
		Assert.assertFalse(test.isPostSaveCalled());
		Assert.assertFalse(((MappedExtensionJoinedStrategyExtension) test.getAggregatedAssociation()).isPreSaveCalled());
		Assert.assertFalse(((MappedExtensionJoinedStrategyExtension) test.getAggregatedAssociation()).isPostSaveCalled());
		Assert.assertFalse(((MappedExtensionJoinedStrategyExtension) test.getAggregatedCollection().get(0)).isPreSaveCalled());
		Assert.assertFalse(((MappedExtensionJoinedStrategyExtension) test.getAggregatedCollection().get(0)).isPostSaveCalled());
		Assert.assertFalse(((MappedExtensionJoinedStrategyExtension) test.getAggregatedCollection().get(1)).isPreSaveCalled());
		Assert.assertFalse(((MappedExtensionJoinedStrategyExtension) test.getAggregatedCollection().get(1)).isPostSaveCalled());

		// Don't assign this coz we wanna test the old transient bean for evidence of bizlet calls
		MappedExtensionJoinedStrategyExtension persistedTest = p.save(test);
		
		Assert.assertTrue(test.isPreSaveCalled());
		Assert.assertTrue(((MappedExtensionJoinedStrategyExtension) test.getAggregatedAssociation()).isPreSaveCalled());
		Assert.assertTrue(((MappedExtensionJoinedStrategyExtension) test.getAggregatedCollection().get(0)).isPreSaveCalled());
		Assert.assertTrue(((MappedExtensionJoinedStrategyExtension) test.getAggregatedCollection().get(1)).isPreSaveCalled());

		// Post save is called on the now managed persistent version
		Assert.assertTrue(persistedTest.isPostSaveCalled());
		Assert.assertTrue(((MappedExtensionJoinedStrategyExtension) persistedTest.getAggregatedCollection().get(0)).isPostSaveCalled());
		Assert.assertTrue(((MappedExtensionJoinedStrategyExtension) persistedTest.getAggregatedAssociation()).isPostSaveCalled());
		Assert.assertTrue(((MappedExtensionJoinedStrategyExtension) persistedTest.getAggregatedCollection().get(1)).isPostSaveCalled());
	}

	@Test
	public void testDerivedPropertiesAvailableAfterSaveESS() throws Exception {
		MappedExtensionSingleStrategyExtension test = Util.constructRandomInstance(u, m, messd, 4);
		testDerivedPropertiesAvailableAfterSave(test);
	}

	@Test
	public void testDerivedPropertiesAvailableAfterSaveSSS() throws Exception {
		MappedSubclassedSingleStrategy test = Util.constructRandomInstance(u, m, msssd, 4);
		testDerivedPropertiesAvailableAfterSave(test);
	}

	private void testDerivedPropertiesAvailableAfterSave(MappedExtensionSingleStrategyExtension test) 
	throws Exception {
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
		
		MappedExtensionSingleStrategyExtension persistedTest = p.save(test);
		
		Assert.assertEquals(values[0], persistedTest.getBaseDerivedInteger());
		Assert.assertEquals(values[1], persistedTest.getDerivedInteger());
		Assert.assertEquals(values[2], persistedTest.getAggregatedAssociation().getBaseDerivedInteger());
		Assert.assertEquals(values[3], persistedTest.getAggregatedAssociation().getDerivedInteger());
		Assert.assertEquals(values[4], persistedTest.getAggregatedCollection().get(0).getBaseDerivedInteger());
		Assert.assertEquals(values[5], persistedTest.getAggregatedCollection().get(0).getDerivedInteger());
		Assert.assertEquals(values[6], persistedTest.getAggregatedCollection().get(1).getBaseDerivedInteger());
		Assert.assertEquals(values[7], persistedTest.getAggregatedCollection().get(1).getDerivedInteger());
	}
	
	@Test
	public void testDerivedPropertiesAvailableAfterSaveEJS() throws Exception {
		MappedExtensionJoinedStrategyExtension test = Util.constructRandomInstance(u, m, mejsd, 4);
		testDerivedPropertiesAvailableAfterSave(test);
	}

	@Test
	public void testDerivedPropertiesAvailableAfterSaveSJS() throws Exception {
		MappedSubclassedJoinedStrategy test = Util.constructRandomInstance(u, m, msjsd, 4);
		testDerivedPropertiesAvailableAfterSave(test);
	}

	private void testDerivedPropertiesAvailableAfterSave(MappedExtensionJoinedStrategyExtension test) 
	throws Exception {
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
		
		MappedExtensionJoinedStrategyExtension persistedTest = p.save(test);
		
		Assert.assertEquals(values[0], persistedTest.getBaseDerivedInteger());
		Assert.assertEquals(values[1], persistedTest.getDerivedInteger());
		Assert.assertEquals(values[2], persistedTest.getAggregatedAssociation().getBaseDerivedInteger());
		Assert.assertEquals(values[3], persistedTest.getAggregatedAssociation().getDerivedInteger());
		Assert.assertEquals(values[4], persistedTest.getAggregatedCollection().get(0).getBaseDerivedInteger());
		Assert.assertEquals(values[5], persistedTest.getAggregatedCollection().get(0).getDerivedInteger());
		Assert.assertEquals(values[6], persistedTest.getAggregatedCollection().get(1).getBaseDerivedInteger());
		Assert.assertEquals(values[7], persistedTest.getAggregatedCollection().get(1).getDerivedInteger());
	}

	@Test
	public void testComposedCascadeDeleteExtensionSingleStrategy() throws Exception {
		MappedExtensionSingleStrategy test = Util.constructRandomInstance(u, m, messd, 2);
		test.setAggregatedAssociation(null);
		test.getAggregatedCollection().clear();
		test = p.save(test);
		
		Assert.assertEquals(4, p.newSQL("select count(1) from TEST_MappedExtensionSingleStrategy").scalarResult(Number.class).intValue()); 
		Assert.assertEquals(2, p.newSQL("select count(1) from TEST_MappedExtensionSingleStrategy_composedCollection").scalarResult(Number.class).intValue()); 
		
		p.delete(test);

		// There should be no TEST_MappedExtension rows in the database since they cascade
		Assert.assertEquals(0, p.newSQL("select count(1) from TEST_MappedExtensionSingleStrategy").scalarResult(Number.class).intValue()); 
		Assert.assertEquals(0, p.newSQL("select count(1) from TEST_MappedExtensionSingleStrategy_composedCollection").scalarResult(Number.class).intValue()); 
	}
	
	@Test
	public void testComposedCascadeDeleteSubclassedSingleStrategy() throws Exception {
		MappedSubclassedSingleStrategy test = Util.constructRandomInstance(u, m, msssd, 2);
		test.setAggregatedAssociation(null);
		test.getAggregatedCollection().clear();
		test = p.save(test);
		
		Assert.assertEquals(4, p.newSQL("select count(1) from TEST_MappedExtensionSingleStrategy").scalarResult(Number.class).intValue()); 
		Assert.assertEquals(2, p.newSQL("select count(1) from TEST_MappedExtensionSingleStrategy_composedCollection").scalarResult(Number.class).intValue()); 
		
		p.delete(test);

		// There should be no TEST_MappedExtension rows in the database since they cascade
		Assert.assertEquals(0, p.newSQL("select count(1) from TEST_MappedExtensionSingleStrategy").scalarResult(Number.class).intValue()); 
		Assert.assertEquals(0, p.newSQL("select count(1) from TEST_MappedExtensionSingleStrategy_composedCollection").scalarResult(Number.class).intValue()); 
	}

	@Test
	public void testComposedCascadeDeleteExtensionJoinedStrategy() throws Exception {
		MappedExtensionJoinedStrategy test = Util.constructRandomInstance(u, m, mejsd, 2);
		test.setAggregatedAssociation(null);
		test.getAggregatedCollection().clear();
		test = p.save(test);
		
		Assert.assertEquals(3, p.newSQL("select count(1) from TEST_MappedExtensionJoinedStrategy").scalarResult(Number.class).intValue()); 
		Assert.assertEquals(2, p.newSQL("select count(1) from TEST_MappedExtensionJoinedStrategy_composedCollection").scalarResult(Number.class).intValue()); 
		
		p.delete(test);

		// There should be no TEST_MappedExtension rows in the database since they cascade
		Assert.assertEquals(0, p.newSQL("select count(1) from TEST_MappedExtensionJoinedStrategy").scalarResult(Number.class).intValue()); 
		Assert.assertEquals(0, p.newSQL("select count(1) from TEST_MappedExtensionJoinedStrategy_composedCollection").scalarResult(Number.class).intValue()); 
	}

	@Test
	public void testComposedCascadeDeleteSubclassedJoinedStrategy() throws Exception {
		MappedSubclassedJoinedStrategy test = Util.constructRandomInstance(u, m, msjsd, 2);
		test.setAggregatedAssociation(null);
		test.getAggregatedCollection().clear();
		test = p.save(test);
		
		Assert.assertEquals(3, p.newSQL("select count(1) from TEST_MappedExtensionJoinedStrategy").scalarResult(Number.class).intValue()); 
		Assert.assertEquals(1, p.newSQL("select count(1) from TEST_MappedSubclassedJoinedStrategy").scalarResult(Number.class).intValue()); 
		Assert.assertEquals(2, p.newSQL("select count(1) from TEST_MappedExtensionJoinedStrategy_composedCollection").scalarResult(Number.class).intValue()); 
		
		p.delete(test);

		// There should be no TEST_MappedExtension rows in the database since they cascade
		Assert.assertEquals(0, p.newSQL("select count(1) from TEST_MappedExtensionJoinedStrategy").scalarResult(Number.class).intValue()); 
		Assert.assertEquals(0, p.newSQL("select count(1) from TEST_MappedSubclassedJoinedStrategy").scalarResult(Number.class).intValue()); 
		Assert.assertEquals(0, p.newSQL("select count(1) from TEST_MappedExtensionJoinedStrategy_composedCollection").scalarResult(Number.class).intValue()); 
	}

	@Test
	public void testAggregatedCascadeDeleteExtensionSingleStrategy() throws Exception {
		MappedExtensionSingleStrategy test = Util.constructRandomInstance(u, m, messd, 2);
		test = p.save(test);
		
		Assert.assertEquals(7, p.newSQL("select count(1) from TEST_MappedExtensionSingleStrategy").scalarResult(Number.class).intValue()); 
		Assert.assertEquals(2, p.newSQL("select count(1) from TEST_MappedExtensionSingleStrategy_composedCollection").scalarResult(Number.class).intValue()); 
		Assert.assertEquals(2, p.newSQL("select count(1) from TEST_MappedExtensionSingleStrategy_aggregatedCollection").scalarResult(Number.class).intValue()); 
		
		p.delete(test);

		// There should be 3 TEST_MappedExtension rows in the database since they cascade
		// but the aggregated ones (2 in collections and 1 associations) are left behind
		Assert.assertEquals(3, p.newSQL("select count(1) from TEST_MappedExtensionSingleStrategy").scalarResult(Number.class).intValue()); 
		Assert.assertEquals(0, p.newSQL("select count(1) from TEST_MappedExtensionSingleStrategy_composedCollection").scalarResult(Number.class).intValue()); 
		Assert.assertEquals(0, p.newSQL("select count(1) from TEST_MappedExtensionSingleStrategy_aggregatedCollection").scalarResult(Number.class).intValue()); 
	}

	@Test
	public void testAggregatedCascadeDeleteSubclassedSingleStrategy() throws Exception {
		MappedSubclassedSingleStrategy test = Util.constructRandomInstance(u, m, msssd, 2);
		test = p.save(test);
		
		Assert.assertEquals(7, p.newSQL("select count(1) from TEST_MappedExtensionSingleStrategy").scalarResult(Number.class).intValue()); 
		Assert.assertEquals(2, p.newSQL("select count(1) from TEST_MappedExtensionSingleStrategy_composedCollection").scalarResult(Number.class).intValue()); 
		Assert.assertEquals(2, p.newSQL("select count(1) from TEST_MappedExtensionSingleStrategy_aggregatedCollection").scalarResult(Number.class).intValue()); 
		
		p.delete(test);

		// There should be 3 TEST_MappedExtension rows in the database since they cascade
		// but the aggregated ones (2 in collections and 1 associations) are left behind
		Assert.assertEquals(3, p.newSQL("select count(1) from TEST_MappedExtensionSingleStrategy").scalarResult(Number.class).intValue()); 
		Assert.assertEquals(0, p.newSQL("select count(1) from TEST_MappedExtensionSingleStrategy_composedCollection").scalarResult(Number.class).intValue()); 
		Assert.assertEquals(0, p.newSQL("select count(1) from TEST_MappedExtensionSingleStrategy_aggregatedCollection").scalarResult(Number.class).intValue()); 
	}

	@Test
	public void testAggregatedCascadeDeleteExtensionJoinedStrategy() throws Exception {
		MappedExtensionJoinedStrategy test = Util.constructRandomInstance(u, m, mejsd, 2);
		test = p.save(test);
		
		Assert.assertEquals(6, p.newSQL("select count(1) from TEST_MappedExtensionJoinedStrategy").scalarResult(Number.class).intValue()); 
		Assert.assertEquals(2, p.newSQL("select count(1) from TEST_MappedExtensionJoinedStrategy_composedCollection").scalarResult(Number.class).intValue()); 
		Assert.assertEquals(2, p.newSQL("select count(1) from TEST_MappedExtensionJoinedStrategy_aggregatedCollection").scalarResult(Number.class).intValue()); 
		
		p.delete(test);

		// There should be 3 TEST_MappedExtension rows in the database since they cascade
		// but the aggregated ones (2 in collections and 1 associations) are left behind
		Assert.assertEquals(3, p.newSQL("select count(1) from TEST_MappedExtensionJoinedStrategy").scalarResult(Number.class).intValue()); 
		Assert.assertEquals(0, p.newSQL("select count(1) from TEST_MappedExtensionJoinedStrategy_composedCollection").scalarResult(Number.class).intValue()); 
		Assert.assertEquals(0, p.newSQL("select count(1) from TEST_MappedExtensionJoinedStrategy_aggregatedCollection").scalarResult(Number.class).intValue()); 
	}

	@Test
	public void testAggregatedCascadeDeleteSubclassedJoinedStrategy() throws Exception {
		MappedSubclassedJoinedStrategy test = Util.constructRandomInstance(u, m, msjsd, 2);
		test = p.save(test);
		
		Assert.assertEquals(6, p.newSQL("select count(1) from TEST_MappedExtensionJoinedStrategy").scalarResult(Number.class).intValue()); 
		Assert.assertEquals(1, p.newSQL("select count(1) from TEST_MappedSubclassedJoinedStrategy").scalarResult(Number.class).intValue()); 
		Assert.assertEquals(2, p.newSQL("select count(1) from TEST_MappedExtensionJoinedStrategy_composedCollection").scalarResult(Number.class).intValue()); 
		Assert.assertEquals(2, p.newSQL("select count(1) from TEST_MappedExtensionJoinedStrategy_aggregatedCollection").scalarResult(Number.class).intValue()); 
		
		p.delete(test);

		// There should be 3 TEST_MappedExtension rows in the database since they cascade
		// but the aggregated ones (2 in collections and 1 associations) are left behind
		Assert.assertEquals(3, p.newSQL("select count(1) from TEST_MappedExtensionJoinedStrategy").scalarResult(Number.class).intValue()); 
		Assert.assertEquals(0, p.newSQL("select count(1) from TEST_MappedSubclassedJoinedStrategy").scalarResult(Number.class).intValue()); 
		Assert.assertEquals(0, p.newSQL("select count(1) from TEST_MappedExtensionJoinedStrategy_composedCollection").scalarResult(Number.class).intValue()); 
		Assert.assertEquals(0, p.newSQL("select count(1) from TEST_MappedExtensionJoinedStrategy_aggregatedCollection").scalarResult(Number.class).intValue()); 
	}

	@Test(expected = ReferentialConstraintViolationException.class)
	public void testAggregatedAssociationReferentialIntegritySingleStrategy() throws Exception {
		MappedExtensionSingleStrategy test = Util.constructRandomInstance(u, m, messd, 2);
		test = p.save(test);
		
		p.delete(test.getAggregatedAssociation());
	}

	@Test(expected = ReferentialConstraintViolationException.class)
	public void testAggregatedCollectionReferentialIntegritySingleStrategy() throws Exception {
		MappedExtensionSingleStrategy test = Util.constructRandomInstance(u, m, messd, 2);
		test = p.save(test);
		
		p.delete(test.getAggregatedCollection().get(0));
	}

	@Test(expected = ReferentialConstraintViolationException.class)
	public void testComposedAssociationReferentialIntegritySingleStrategy() throws Exception {
		MappedExtensionSingleStrategy test = Util.constructRandomInstance(u, m, messd, 2);
		test = p.save(test);
		
		p.delete(test.getComposedAssociation());
	}

	@Test(expected = ReferentialConstraintViolationException.class)
	public void testComposedCollectionReferentialIntegritySingleStrategy() throws Exception {
		MappedExtensionSingleStrategy test = Util.constructRandomInstance(u, m, messd, 2);
		test = p.save(test);
		
		p.delete(test.getComposedCollection().get(0));
	}

	@Test(expected = ReferentialConstraintViolationException.class)
	public void testAggregatedAssociationReferentialIntegrityJoinedStrategy() throws Exception {
		MappedExtensionJoinedStrategy test = Util.constructRandomInstance(u, m, mejsd, 2);
		test = p.save(test);
		
		p.delete(test.getAggregatedAssociation());
	}

	@Test(expected = ReferentialConstraintViolationException.class)
	public void testAggregatedCollectionReferentialIntegrityJoinedStrategy() throws Exception {
		MappedExtensionJoinedStrategy test = Util.constructRandomInstance(u, m, mejsd, 2);
		test = p.save(test);
		
		p.delete(test.getAggregatedCollection().get(0));
	}

	@Test(expected = ReferentialConstraintViolationException.class)
	public void testComposedAssociationReferentialIntegrityJoinedStrategy() throws Exception {
		MappedExtensionJoinedStrategy test = Util.constructRandomInstance(u, m, mejsd, 2);
		test = p.save(test);
		
		p.delete(test.getComposedAssociation());
	}

	@Test(expected = ReferentialConstraintViolationException.class)
	public void testComposedCollectionReferentialIntegrityJoinedStrategy() throws Exception {
		MappedExtensionJoinedStrategy test = Util.constructRandomInstance(u, m, mejsd, 2);
		test = p.save(test);
		
		p.delete(test.getComposedCollection().get(0));
	}
}