package modules.test;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.hibernate.Session;
import org.junit.Assert;
import org.junit.Test;
import org.skyve.CORE;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.messages.OptimisticLockException;
import org.skyve.domain.types.OptimisticLock;
import org.skyve.impl.domain.messages.ReferentialConstraintViolationException;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.persistence.hibernate.AbstractHibernatePersistence;
import org.skyve.persistence.SQL;
import org.skyve.util.Util;

import modules.test.MappedExtensionJoinedStrategy.MappedExtensionJoinedStrategyExtension;
import modules.test.MappedExtensionSingleStrategy.MappedExtensionSingleStrategyExtension;
import modules.test.domain.AllAttributesPersistent;
import modules.test.domain.MappedExtensionJoinedStrategy;
import modules.test.domain.MappedExtensionSingleStrategy;
import modules.test.domain.MappedSubclassedJoinedStrategy;
import modules.test.domain.MappedSubclassedSingleStrategy;

public class PersistenceTests extends AbstractSkyveTest {
	@Test
	public void testPersistenceOfObjectWithReferenceToAnotherObjectWithAggregatedCollectionWithCascadeMergeOn() throws Exception {
		AllAttributesPersistent test = Util.constructRandomInstance(u, m, aapd, 3);
		test = p.save(test);
		test = p.save(test);

		Assert.assertEquals(Integer.valueOf(0), test.getAggregatedCollection().get(0).getBizVersion());
		Assert.assertEquals(Integer.valueOf(0), test.getAggregatedCollection().get(1).getBizVersion());
	}

	/**
	 * Make a random instance, save it, clone it and save the copy.
	 * @throws Exception
	 */
	@Test
	public void testMergeOfDetached() throws Exception {
		AllAttributesPersistent test = Util.constructRandomInstance(u, m, aapd, 3);
		test = p.save(test);
		test = Util.cloneToTransientBySerialization(test);
		test = p.save(test);
	}

	/**
	 * Make a random instance, save it, start a new persistence, clone it and save the copy.
	 * @throws Exception
	 */
	@Test
	public void testMergeOfDetachedWithFreshLoad() throws Exception {
		AllAttributesPersistent test = Util.constructRandomInstance(u, m, aapd, 3);
		test = p.save(test);
		p.evictAllCached();
		p.commit(true);
		p = CORE.getPersistence();
		((AbstractPersistence) p).setUser(u);
		p.begin();
		test = p.retrieve(aapd, test.getBizId(), false);
		test = Util.cloneToTransientBySerialization(test);
		test = p.save(test);
	}
	
	/**
	 * Create a hub which is pointed to by 
	 * intermediate1, pointed to by spoke1 and 
	 * intermediate2, pointed to by spoke 2.
	 * Save spoke1 and spoke2.
	 * Retrieve spoke1, clone it and save it.
	 * @throws Exception
	 */
	@Test
	public void testMergeOfDetachedWithHubEntityAndTwoLevelsOfIndirection() throws Exception {
		AllAttributesPersistent hub = Util.constructRandomInstance(u, m, aapd, 0);
		AllAttributesPersistent intermediate1 = Util.constructRandomInstance(u, m, aapd, 0);
		intermediate1.setAggregatedAssociation(hub);
		AllAttributesPersistent spoke1 = Util.constructRandomInstance(u, m, aapd, 0);
		spoke1.setAggregatedAssociation(intermediate1);
		spoke1 = p.save(spoke1);
		hub = spoke1.getAggregatedAssociation();
		AllAttributesPersistent intermediate2 = Util.constructRandomInstance(u, m, aapd, 0);
		intermediate2.setAggregatedAssociation(hub);
		AllAttributesPersistent spoke2 = Util.constructRandomInstance(u, m, aapd, 0);
		spoke2.setAggregatedAssociation(intermediate2);
		spoke2 = p.save(spoke2);

		p.evictAllCached();
		
		spoke1 = p.retrieve(aapd, spoke1.getBizId(), false);
		AllAttributesPersistent spoke3 = Util.cloneToTransientBySerialization(spoke1);
		spoke3 = p.save(spoke3);
		
		Assert.assertEquals(hub, spoke3.getAggregatedAssociation());
	}
	
	/**
	 * This test should throw as hibernate cannot flush because the result of the merge operation has not been assigned back to its parent object.
	 * A different object with the same identifier value was already associated with the session : [testAllAttributesPersistent#<bizId>]
	 * @throws Exception
	 */
	@Test(expected = DomainException.class)
	public void testPartialSave() throws Exception {
		AllAttributesPersistent test = Util.constructRandomInstance(u, m, aapd, 3);
		test = p.save(test);
		test.setAggregatedAssociation(Util.constructRandomInstance(u, m, aapd, 0));
		test.setAggregatedAssociation(p.save(test.getAggregatedAssociation()));
	}

	/**
	 * This test should not throw as the result of the merge has been assigned back to its parent object.
	 * Then flush is called.
	 * @throws Exception
	 */
	@Test
	public void testPartialMerge() throws Exception {
		AllAttributesPersistent test = Util.constructRandomInstance(u, m, aapd, 3);
		test = p.save(test);
		test.setAggregatedAssociation(Util.constructRandomInstance(u, m, aapd, 0));
		test.setAggregatedAssociation(p.merge(test.getAggregatedAssociation()));
		p.flush();
	}

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
		Assert.assertFalse(test.getAggregatedAssociation().isPreSaveCalled());
		Assert.assertFalse(test.getAggregatedAssociation().isPostSaveCalled());
		Assert.assertFalse(test.getComposedAssociation().isPreSaveCalled());
		Assert.assertFalse(test.getComposedAssociation().isPostSaveCalled());
		Assert.assertFalse(test.getAggregatedCollection().get(0).isPreSaveCalled());
		Assert.assertFalse(test.getAggregatedCollection().get(0).isPostSaveCalled());
		Assert.assertFalse(test.getAggregatedCollection().get(1).isPreSaveCalled());
		Assert.assertFalse(test.getAggregatedCollection().get(1).isPostSaveCalled());

		// Don't assign this coz we wanna test the old transient bean for evidence of bizlet calls
		MappedExtensionSingleStrategyExtension persistedTest = p.save(test);

		Assert.assertTrue(test.isPreSaveCalled());
		Assert.assertTrue(test.getAggregatedAssociation().isPreSaveCalled());
		Assert.assertTrue(test.getComposedAssociation().isPreSaveCalled());
		Assert.assertTrue(test.getAggregatedCollection().get(0).isPreSaveCalled());
		Assert.assertTrue(test.getAggregatedCollection().get(1).isPreSaveCalled());

		// Post save is called on the now managed persistent version
		Assert.assertTrue(persistedTest.isPostSaveCalled());
		Assert.assertTrue(persistedTest.getAggregatedAssociation().isPostSaveCalled());
		Assert.assertTrue(persistedTest.getComposedAssociation().isPostSaveCalled());
		Assert.assertTrue(persistedTest.getAggregatedCollection().get(0).isPostSaveCalled());
		Assert.assertTrue(persistedTest.getAggregatedCollection().get(1).isPostSaveCalled());
	}

	@Test
	public void testPreAndPostSaveNoBaseEJS() throws Exception {
		MappedExtensionJoinedStrategyExtension test = Util.constructRandomInstance(u, m, mejsd, 3);

		Assert.assertFalse(test.isPreSaveCalled());
		Assert.assertFalse(test.isPostSaveCalled());
		Assert.assertFalse(test.getAggregatedAssociation().isPreSaveCalled());
		Assert.assertFalse(test.getAggregatedAssociation().isPostSaveCalled());
		Assert.assertFalse(test.getComposedAssociation().isPreSaveCalled());
		Assert.assertFalse(test.getComposedAssociation().isPostSaveCalled());
		Assert.assertFalse(test.getAggregatedCollection().get(0).isPreSaveCalled());
		Assert.assertFalse(test.getAggregatedCollection().get(0).isPostSaveCalled());
		Assert.assertFalse(test.getAggregatedCollection().get(1).isPreSaveCalled());
		Assert.assertFalse(test.getAggregatedCollection().get(1).isPostSaveCalled());

		// Don't assign this coz we wanna test the old transient bean for evidence of bizlet calls
		MappedExtensionJoinedStrategyExtension persistedTest = p.save(test);

		Assert.assertTrue(test.isPreSaveCalled());
		Assert.assertTrue(test.getAggregatedAssociation().isPreSaveCalled());
		Assert.assertTrue(test.getComposedAssociation().isPreSaveCalled());
		Assert.assertTrue(test.getAggregatedCollection().get(0).isPreSaveCalled());
		Assert.assertTrue(test.getAggregatedCollection().get(1).isPreSaveCalled());

		// Post save is called on the now managed persistent version
		Assert.assertTrue(persistedTest.isPostSaveCalled());
		Assert.assertTrue(persistedTest.getAggregatedAssociation().isPostSaveCalled());
		Assert.assertTrue(persistedTest.getComposedAssociation().isPostSaveCalled());
		Assert.assertTrue(persistedTest.getAggregatedCollection().get(0).isPostSaveCalled());
		Assert.assertTrue(persistedTest.getAggregatedCollection().get(1).isPostSaveCalled());
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
		Integer[] values = new Integer[] { test.getBaseDerivedInteger(),
				test.getDerivedInteger(),
				test.getAggregatedAssociation().getBaseDerivedInteger(),
				test.getAggregatedAssociation().getDerivedInteger(),
				test.getAggregatedCollection().get(0).getBaseDerivedInteger(),
				test.getAggregatedCollection().get(0).getDerivedInteger(),
				test.getAggregatedCollection().get(1).getBaseDerivedInteger(),
				test.getAggregatedCollection().get(1).getDerivedInteger() };
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
		Integer[] values = new Integer[] { test.getBaseDerivedInteger(),
				test.getDerivedInteger(),
				test.getAggregatedAssociation().getBaseDerivedInteger(),
				test.getAggregatedAssociation().getDerivedInteger(),
				test.getAggregatedCollection().get(0).getBaseDerivedInteger(),
				test.getAggregatedCollection().get(0).getDerivedInteger(),
				test.getAggregatedCollection().get(1).getBaseDerivedInteger(),
				test.getAggregatedCollection().get(1).getDerivedInteger() };
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
	public void testAggregatedAssociationDoesntCascadeDelete() throws Exception {
		AllAttributesPersistent test = Util.constructRandomInstance(u, m, aapd, 2);
		test.setComposedAssociation(null);
		test.getAggregatedCollection().clear();
		test = p.save(test);
		String associationBizId = test.getAggregatedAssociation().getBizId();
		Assert.assertEquals(2, p.newSQL("select count(1) from TEST_AllAttributesPersistent").scalarResult(Number.class).intValue());
		p.delete(test);
		Assert.assertEquals(associationBizId, p.newSQL("select bizId from TEST_AllAttributesPersistent").scalarResult(String.class));
	}

	@Test
	public void testComposedAssociationDoesCascadeDelete() throws Exception {
		AllAttributesPersistent test = Util.constructRandomInstance(u, m, aapd, 2);
		test.setAggregatedAssociation(null);
		test.getAggregatedCollection().clear();
		test = p.save(test);
		Assert.assertEquals(2, p.newSQL("select count(1) from TEST_AllAttributesPersistent").scalarResult(Number.class).intValue());
		p.delete(test);
		Assert.assertEquals(0, p.newSQL("select count(1) from TEST_AllAttributesPersistent").scalarResult(Number.class).intValue());
	}

	@Test
	public void testComposedAssociationNulledDoesCascadeDelete() throws Exception {
		AllAttributesPersistent test = Util.constructRandomInstance(u, m, aapd, 2);
		test.setAggregatedAssociation(null);
		test.getAggregatedCollection().clear();
		test = p.save(test);
		Assert.assertEquals(2, p.newSQL("select count(1) from TEST_AllAttributesPersistent").scalarResult(Number.class).intValue());
		test.setComposedAssociation(null);
		p.save(test);
		Assert.assertEquals(1, p.newSQL("select count(1) from TEST_AllAttributesPersistent").scalarResult(Number.class).intValue());
	}

	@Test
	public void testComposedCascadeDeleteExtensionSingleStrategy() throws Exception {
		MappedExtensionSingleStrategy test = Util.constructRandomInstance(u, m, messd, 2);
		test.setAggregatedAssociation(null);
		test.getAggregatedCollection().clear();
		test = p.save(test);

		Assert.assertEquals(4,
				p.newSQL("select count(1) from TEST_MappedExtensionSingleStrategy").scalarResult(Number.class).intValue());
		Assert.assertEquals(2, p.newSQL("select count(1) from TEST_MappedExtensionSingleStrategy_composedCollection")
				.scalarResult(Number.class).intValue());

		p.delete(test);

		// There should be no TEST_MappedExtension rows in the database since they cascade
		Assert.assertEquals(0,
				p.newSQL("select count(1) from TEST_MappedExtensionSingleStrategy").scalarResult(Number.class).intValue());
		Assert.assertEquals(0, p.newSQL("select count(1) from TEST_MappedExtensionSingleStrategy_composedCollection")
				.scalarResult(Number.class).intValue());
	}

	@Test
	public void testComposedCascadeDeleteSubclassedSingleStrategy() throws Exception {
		MappedSubclassedSingleStrategy test = Util.constructRandomInstance(u, m, msssd, 2);
		test.setAggregatedAssociation(null);
		test.getAggregatedCollection().clear();
		test = p.save(test);

		Assert.assertEquals(4,
				p.newSQL("select count(1) from TEST_MappedExtensionSingleStrategy").scalarResult(Number.class).intValue());
		Assert.assertEquals(2, p.newSQL("select count(1) from TEST_MappedExtensionSingleStrategy_composedCollection")
				.scalarResult(Number.class).intValue());

		p.delete(test);

		// There should be no TEST_MappedExtension rows in the database since they cascade
		Assert.assertEquals(0,
				p.newSQL("select count(1) from TEST_MappedExtensionSingleStrategy").scalarResult(Number.class).intValue());
		Assert.assertEquals(0, p.newSQL("select count(1) from TEST_MappedExtensionSingleStrategy_composedCollection")
				.scalarResult(Number.class).intValue());
	}

	@Test
	public void testComposedCascadeDeleteExtensionJoinedStrategy() throws Exception {
		MappedExtensionJoinedStrategy test = Util.constructRandomInstance(u, m, mejsd, 2);
		test.setAggregatedAssociation(null);
		test.getAggregatedCollection().clear();
		test = p.save(test);

		Assert.assertEquals(3,
				p.newSQL("select count(1) from TEST_MappedExtensionJoinedStrategy").scalarResult(Number.class).intValue());
		Assert.assertEquals(2, p.newSQL("select count(1) from TEST_MappedExtensionJoinedStrategy_composedCollection")
				.scalarResult(Number.class).intValue());

		p.delete(test);

		// There should be no TEST_MappedExtension rows in the database since they cascade
		Assert.assertEquals(0,
				p.newSQL("select count(1) from TEST_MappedExtensionJoinedStrategy").scalarResult(Number.class).intValue());
		Assert.assertEquals(0, p.newSQL("select count(1) from TEST_MappedExtensionJoinedStrategy_composedCollection")
				.scalarResult(Number.class).intValue());
	}

	@Test
	public void testComposedCascadeDeleteSubclassedJoinedStrategy() throws Exception {
		MappedSubclassedJoinedStrategy test = Util.constructRandomInstance(u, m, msjsd, 2);
		test.setAggregatedAssociation(null);
		test.getAggregatedCollection().clear();
		test = p.save(test);

		Assert.assertEquals(3,
				p.newSQL("select count(1) from TEST_MappedExtensionJoinedStrategy").scalarResult(Number.class).intValue());
		Assert.assertEquals(1,
				p.newSQL("select count(1) from TEST_MappedSubclassedJoinedStrategy").scalarResult(Number.class).intValue());
		Assert.assertEquals(2, p.newSQL("select count(1) from TEST_MappedExtensionJoinedStrategy_composedCollection")
				.scalarResult(Number.class).intValue());

		p.delete(test);

		// There should be no TEST_MappedExtension rows in the database since they cascade
		Assert.assertEquals(0,
				p.newSQL("select count(1) from TEST_MappedExtensionJoinedStrategy").scalarResult(Number.class).intValue());
		Assert.assertEquals(0,
				p.newSQL("select count(1) from TEST_MappedSubclassedJoinedStrategy").scalarResult(Number.class).intValue());
		Assert.assertEquals(0, p.newSQL("select count(1) from TEST_MappedExtensionJoinedStrategy_composedCollection")
				.scalarResult(Number.class).intValue());
	}

	@Test
	public void testAggregatedCascadeDeleteExtensionSingleStrategy() throws Exception {
		MappedExtensionSingleStrategy test = Util.constructRandomInstance(u, m, messd, 2);
		test = p.save(test);

		Assert.assertEquals(7,
				p.newSQL("select count(1) from TEST_MappedExtensionSingleStrategy").scalarResult(Number.class).intValue());
		Assert.assertEquals(2, p.newSQL("select count(1) from TEST_MappedExtensionSingleStrategy_composedCollection")
				.scalarResult(Number.class).intValue());
		Assert.assertEquals(2, p.newSQL("select count(1) from TEST_MappedExtensionSingleStrategy_aggregatedCollection")
				.scalarResult(Number.class).intValue());

		p.delete(test);

		// There should be 3 TEST_MappedExtension rows in the database since they cascade
		// but the aggregated ones (2 in collections and 1 associations) are left behind
		Assert.assertEquals(3,
				p.newSQL("select count(1) from TEST_MappedExtensionSingleStrategy").scalarResult(Number.class).intValue());
		Assert.assertEquals(0, p.newSQL("select count(1) from TEST_MappedExtensionSingleStrategy_composedCollection")
				.scalarResult(Number.class).intValue());
		Assert.assertEquals(0, p.newSQL("select count(1) from TEST_MappedExtensionSingleStrategy_aggregatedCollection")
				.scalarResult(Number.class).intValue());
	}

	@Test
	public void testAggregatedCascadeDeleteSubclassedSingleStrategy() throws Exception {
		MappedSubclassedSingleStrategy test = Util.constructRandomInstance(u, m, msssd, 2);
		test = p.save(test);

		Assert.assertEquals(7,
				p.newSQL("select count(1) from TEST_MappedExtensionSingleStrategy").scalarResult(Number.class).intValue());
		Assert.assertEquals(2, p.newSQL("select count(1) from TEST_MappedExtensionSingleStrategy_composedCollection")
				.scalarResult(Number.class).intValue());
		Assert.assertEquals(2, p.newSQL("select count(1) from TEST_MappedExtensionSingleStrategy_aggregatedCollection")
				.scalarResult(Number.class).intValue());

		p.delete(test);

		// There should be 3 TEST_MappedExtension rows in the database since they cascade
		// but the aggregated ones (2 in collections and 1 associations) are left behind
		Assert.assertEquals(3,
				p.newSQL("select count(1) from TEST_MappedExtensionSingleStrategy").scalarResult(Number.class).intValue());
		Assert.assertEquals(0, p.newSQL("select count(1) from TEST_MappedExtensionSingleStrategy_composedCollection")
				.scalarResult(Number.class).intValue());
		Assert.assertEquals(0, p.newSQL("select count(1) from TEST_MappedExtensionSingleStrategy_aggregatedCollection")
				.scalarResult(Number.class).intValue());
	}

	@Test
	public void testAggregatedCascadeDeleteExtensionJoinedStrategy() throws Exception {
		MappedExtensionJoinedStrategy test = Util.constructRandomInstance(u, m, mejsd, 2);
		test = p.save(test);

		Assert.assertEquals(6,
				p.newSQL("select count(1) from TEST_MappedExtensionJoinedStrategy").scalarResult(Number.class).intValue());
		Assert.assertEquals(2, p.newSQL("select count(1) from TEST_MappedExtensionJoinedStrategy_composedCollection")
				.scalarResult(Number.class).intValue());
		Assert.assertEquals(2, p.newSQL("select count(1) from TEST_MappedExtensionJoinedStrategy_aggregatedCollection")
				.scalarResult(Number.class).intValue());

		p.delete(test);

		// There should be 3 TEST_MappedExtension rows in the database since they cascade
		// but the aggregated ones (2 in collections and 1 associations) are left behind
		Assert.assertEquals(3,
				p.newSQL("select count(1) from TEST_MappedExtensionJoinedStrategy").scalarResult(Number.class).intValue());
		Assert.assertEquals(0, p.newSQL("select count(1) from TEST_MappedExtensionJoinedStrategy_composedCollection")
				.scalarResult(Number.class).intValue());
		Assert.assertEquals(0, p.newSQL("select count(1) from TEST_MappedExtensionJoinedStrategy_aggregatedCollection")
				.scalarResult(Number.class).intValue());
	}

	@Test
	public void testAggregatedCascadeDeleteSubclassedJoinedStrategy() throws Exception {
		MappedSubclassedJoinedStrategy test = Util.constructRandomInstance(u, m, msjsd, 2);
		test = p.save(test);

		Assert.assertEquals(6,
				p.newSQL("select count(1) from TEST_MappedExtensionJoinedStrategy").scalarResult(Number.class).intValue());
		Assert.assertEquals(1,
				p.newSQL("select count(1) from TEST_MappedSubclassedJoinedStrategy").scalarResult(Number.class).intValue());
		Assert.assertEquals(2, p.newSQL("select count(1) from TEST_MappedExtensionJoinedStrategy_composedCollection")
				.scalarResult(Number.class).intValue());
		Assert.assertEquals(2, p.newSQL("select count(1) from TEST_MappedExtensionJoinedStrategy_aggregatedCollection")
				.scalarResult(Number.class).intValue());

		p.delete(test);

		// There should be 3 TEST_MappedExtension rows in the database since they cascade
		// but the aggregated ones (2 in collections and 1 associations) are left behind
		Assert.assertEquals(3,
				p.newSQL("select count(1) from TEST_MappedExtensionJoinedStrategy").scalarResult(Number.class).intValue());
		Assert.assertEquals(0,
				p.newSQL("select count(1) from TEST_MappedSubclassedJoinedStrategy").scalarResult(Number.class).intValue());
		Assert.assertEquals(0, p.newSQL("select count(1) from TEST_MappedExtensionJoinedStrategy_composedCollection")
				.scalarResult(Number.class).intValue());
		Assert.assertEquals(0, p.newSQL("select count(1) from TEST_MappedExtensionJoinedStrategy_aggregatedCollection")
				.scalarResult(Number.class).intValue());
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

	@Test
	public void testComposedCollectionRemoveMemberSingleStrategy() throws Exception {
		MappedExtensionSingleStrategy test = Util.constructRandomInstance(u, m, messd, 2);
		test = p.save(test);

		Assert.assertEquals(7,
				p.newSQL("select count(1) from TEST_MappedExtensionSingleStrategy").scalarResult(Number.class).intValue());
		Assert.assertEquals(2, p.newSQL("select count(1) from TEST_MappedExtensionSingleStrategy_composedCollection")
				.scalarResult(Number.class).intValue());
		Assert.assertEquals(2, p.newSQL("select count(1) from TEST_MappedExtensionSingleStrategy_aggregatedCollection")
				.scalarResult(Number.class).intValue());

		test.getComposedCollection().remove(0);

		test = p.save(test);

		// Check the composed collection element got cascaded with no referential integrity troubles
		Assert.assertEquals(6,
				p.newSQL("select count(1) from TEST_MappedExtensionSingleStrategy").scalarResult(Number.class).intValue());
		Assert.assertEquals(1, p.newSQL("select count(1) from TEST_MappedExtensionSingleStrategy_composedCollection")
				.scalarResult(Number.class).intValue());
		Assert.assertEquals(2, p.newSQL("select count(1) from TEST_MappedExtensionSingleStrategy_aggregatedCollection")
				.scalarResult(Number.class).intValue());

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

	@Test
	public void testComposedCollectionRemoveMemberJoinedStrategy() throws Exception {
		MappedExtensionJoinedStrategy test = Util.constructRandomInstance(u, m, mejsd, 2);
		test = p.save(test);

		Assert.assertEquals(6,
				p.newSQL("select count(1) from TEST_MappedExtensionJoinedStrategy").scalarResult(Number.class).intValue());
		Assert.assertEquals(0,
				p.newSQL("select count(1) from TEST_MappedSubclassedJoinedStrategy").scalarResult(Number.class).intValue());
		Assert.assertEquals(2, p.newSQL("select count(1) from TEST_MappedExtensionJoinedStrategy_composedCollection")
				.scalarResult(Number.class).intValue());
		Assert.assertEquals(2, p.newSQL("select count(1) from TEST_MappedExtensionJoinedStrategy_aggregatedCollection")
				.scalarResult(Number.class).intValue());

		test.getComposedCollection().remove(0);

		test = p.save(test);

		// Check the composed collection element got cascaded with no referential integrity troubles
		Assert.assertEquals(5,
				p.newSQL("select count(1) from TEST_MappedExtensionJoinedStrategy").scalarResult(Number.class).intValue());
		Assert.assertEquals(0,
				p.newSQL("select count(1) from TEST_MappedSubclassedJoinedStrategy").scalarResult(Number.class).intValue());
		Assert.assertEquals(1, p.newSQL("select count(1) from TEST_MappedExtensionJoinedStrategy_composedCollection")
				.scalarResult(Number.class).intValue());
		Assert.assertEquals(2, p.newSQL("select count(1) from TEST_MappedExtensionJoinedStrategy_aggregatedCollection")
				.scalarResult(Number.class).intValue());

	}

	@Test(expected = OptimisticLockException.class)
	public void testOptimisticLockException() throws Exception {
		AllAttributesPersistent test = Util.constructRandomInstance(u, m, aapd, 1);
		test = p.save(test);
		SQL sql = p.newSQL(String.format("update %s set %s = :%s, %s = :%s",
				aapd.getPersistent().getPersistentIdentifier(),
				PersistentBean.LOCK_NAME,
				PersistentBean.LOCK_NAME,
				PersistentBean.VERSION_NAME,
				PersistentBean.VERSION_NAME));
		sql.putParameter(PersistentBean.LOCK_NAME, new OptimisticLock(u.getName(), new Date()).toString(), false);
		sql.putParameter(PersistentBean.VERSION_NAME, Integer.valueOf(2));
		sql.execute();

		test.setText("optimistic lock test");
		p.save(test);
	}

	@Test(expected = OptimisticLockException.class)
	public void testTransientStaleObjectStateExceptionOptimisticLock() throws Exception {
		AllAttributesPersistent test = Util.constructRandomInstance(u, m, aapd, 1);
		p.save(test); // NB not returned
		p.save(test);
	}

	@Test(expected = OptimisticLockException.class)
	public void testDetachedStaleObjectStateExceptionOptimisticLock() throws Exception {
		AllAttributesPersistent test = Util.constructRandomInstance(u, m, aapd, 1);
		test = p.save(test);

		p.evictCached(test);

		test.setText("optimistic lock test");
		p.save(test); // NB not returned

		test.setText("optimistic lock test take 2");
		p.save(test);
	}

	@Test(expected = OptimisticLockException.class)
	public void testClonedStaleObjectStateExceptionOptimisticLock() throws Exception {
		AllAttributesPersistent test = Util.constructRandomInstance(u, m, aapd, 1);
		test = p.save(test);

		test = Util.cloneToTransientBySerialization(test);

		test.setText("optimistic lock test");
		p.save(test); // NB not returned

		test.setText("optimistic lock test take 2");
		p.save(test);
	}

	@Test
	public void testRefresh() throws Exception {
		AllAttributesPersistent test = Util.constructRandomInstance(u, m, aapd, 1);
		test = p.save(test);
		test.setText("optimistic lock test");
		p.refresh(test);
		Assert.assertNotEquals("optimistic lock test", test.getText());
	}

	@Test
	public void testRefreshTransient() throws Exception {
		AllAttributesPersistent test = Util.constructRandomInstance(u, m, aapd, 1);
		p.refresh(test);
	}

	@Test(expected = DomainException.class)
	public void testRefreshDetached() throws Exception {
		AllAttributesPersistent test = Util.constructRandomInstance(u, m, aapd, 1);
		test = p.save(test);
		p.evictCached(test);
		p.refresh(test);
	}

	@Test
	public void testEvict() throws Exception {
		AllAttributesPersistent test = Util.constructRandomInstance(u, m, aapd, 1);
		@SuppressWarnings("resource")
		Session s = ((AbstractHibernatePersistence) p).getSession();
		Assert.assertFalse(s.contains(test.getBizModule() + test.getBizDocument(), test));
		test = p.save(test);
		Assert.assertTrue(s.contains(test.getBizModule() + test.getBizDocument(), test));
		p.evictCached(test);
		Assert.assertFalse(s.contains(test.getBizModule() + test.getBizDocument(), test));
	}
	
	@Test
	public void testEvictEvicted() throws Exception {
		AllAttributesPersistent test = Util.constructRandomInstance(u, m, aapd, 1);
		@SuppressWarnings("resource")
		Session s = ((AbstractHibernatePersistence) p).getSession();
		Assert.assertFalse(s.contains(test.getBizModule() + test.getBizDocument(), test));
		test = p.save(test);
		Assert.assertTrue(s.contains(test.getBizModule() + test.getBizDocument(), test));
		p.evictCached(test);
		Assert.assertFalse(s.contains(test.getBizModule() + test.getBizDocument(), test));
		p.evictCached(test);
		Assert.assertFalse(s.contains(test.getBizModule() + test.getBizDocument(), test));
	}

	@Test
	public void testEvictTransient() throws Exception {
		AllAttributesPersistent test = Util.constructRandomInstance(u, m, aapd, 1);
		@SuppressWarnings("resource")
		Session s = ((AbstractHibernatePersistence) p).getSession();
		Assert.assertFalse(s.contains(test.getBizModule() + test.getBizDocument(), test));
		p.evictCached(test);
		Assert.assertFalse(s.contains(test.getBizModule() + test.getBizDocument(), test));
	}
	
	@Test
	public void testSaveOfList() throws Exception {
		AllAttributesPersistent test1 = Util.constructRandomInstance(u, m, aapd, 1);
		AllAttributesPersistent test2 = Util.constructRandomInstance(u, m, aapd, 1);

		List<AllAttributesPersistent> tests = new ArrayList<>();
		tests.add(test1);
		tests.add(test2);
		tests = p.save(tests);
		Assert.assertEquals(2, tests.size());
		Assert.assertEquals(test1.getBizId(), tests.get(0).getBizId());
		Assert.assertEquals(test2.getBizId(), tests.get(1).getBizId());
		Assert.assertEquals(2, p.newSQL("select count(1) from TEST_AllAttributesPersistent").scalarResult(Number.class).intValue());
		for (AllAttributesPersistent test : tests) {
			Assert.assertTrue(test.isPersisted());
		}
	}

	@Test
	public void testSaveOfVarargs() throws Exception {
		AllAttributesPersistent test1 = Util.constructRandomInstance(u, m, aapd, 1);
		AllAttributesPersistent test2 = Util.constructRandomInstance(u, m, aapd, 1);

		List<AllAttributesPersistent> tests = p.save(test1, test2);
		Assert.assertEquals(2, tests.size());
		Assert.assertEquals(test1.getBizId(), tests.get(0).getBizId());
		Assert.assertEquals(test2.getBizId(), tests.get(1).getBizId());
		Assert.assertEquals(2, p.newSQL("select count(1) from TEST_AllAttributesPersistent").scalarResult(Number.class).intValue());
		for (AllAttributesPersistent test : tests) {
			Assert.assertTrue(test.isPersisted());
		}
	}
}