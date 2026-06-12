package modules.test;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.apache.commons.beanutils.DynaBean;
import org.hibernate.Session;
import org.junit.jupiter.api.Test;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.GeometryFactory;
import org.locationtech.jts.geom.Polygon;
import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.messages.NoResultsException;
import org.skyve.domain.messages.OptimisticLockException;
import org.skyve.domain.messages.ReferentialConstraintViolationException;
import org.skyve.domain.types.OptimisticLock;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.persistence.hibernate.AbstractHibernatePersistence;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.user.DocumentPermissionScope;
import org.skyve.persistence.AutoClosingIterable;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.SQL;
import org.skyve.util.Binder;
import org.skyve.util.Util;

import modules.test.MappedExtensionJoinedStrategy.MappedExtensionJoinedStrategyExtension;
import modules.test.MappedExtensionSingleStrategy.MappedExtensionSingleStrategyExtension;
import modules.test.domain.AllAttributesPersistent;
import modules.test.domain.DeleteDuringPostDelete;
import modules.test.domain.MappedExtensionJoinedStrategy;
import modules.test.domain.MappedExtensionSingleStrategy;
import modules.test.domain.MappedSubclassedJoinedStrategy;
import modules.test.domain.MappedSubclassedSingleStrategy;
import modules.test.domain.Reachability;

@SuppressWarnings({"java:S5778", "java:S5961", "java:S1874", "java:S1130", "java:S2699", "java:S8692", "null"}) // system clock OK
class PersistenceTests extends AbstractSkyveTestDispose {

	@Test
	void testPersistenceOfObjectWithReferenceToAnotherObjectWithAggregatedCollectionWithCascadeMergeOn() throws Exception {
		AllAttributesPersistent test = Util.constructRandomInstance(u, m, aapd, 2);
		test = p.save(test);
		test = p.save(test);
		test = p.save(test);
		test = p.save(test);
		test = p.save(test);
		test = p.save(test);
		test = p.save(test);
		test = p.save(test);

		assertEquals(Integer.valueOf(1), test.getBizVersion());
		assertEquals(Integer.valueOf(0), test.getAggregatedCollection().get(0).getBizVersion());
		assertEquals(Integer.valueOf(0), test.getAggregatedCollection().get(1).getBizVersion());
	}

	/**
	 * Make a random instance, save it, clone it and save the copy.
	 * @throws Exception
	 */
	@Test
	@SuppressWarnings("java:S1854") // Persistence.save() may return a different instance; assignment captured intentionally
	void testMergeOfDetached() throws Exception {
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
	@SuppressWarnings("java:S1854") // Persistence.save() may return a different instance; assignment captured intentionally
	void testMergeOfDetachedWithFreshLoad() throws Exception {
		AllAttributesPersistent test = Util.constructRandomInstance(u, m, aapd, 3);
		test = p.save(test);
		p.evictAllCached();
		p.commit(true);
		p = CORE.getPersistence();
		((AbstractPersistence) p).setUser(u);
		p.begin();
		test = p.retrieve(aapd, test.getBizId());
		assertNotNull(test);
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
	@SuppressWarnings("java:S1854") // Persistence.save() may return a different instance; assignment captured intentionally
	void testMergeOfDetachedWithHubEntityAndTwoLevelsOfIndirection() throws Exception {
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
		
		spoke1 = p.retrieve(aapd, spoke1.getBizId());
		assertNotNull(spoke1);
		AllAttributesPersistent spoke3 = Util.cloneToTransientBySerialization(spoke1);
		spoke3 = p.save(spoke3);
		
		assertEquals(hub, spoke3.getAggregatedAssociation());
	}
	
	/**
	 * This test should throw as hibernate cannot flush because the result of the merge operation has not been assigned back to its parent object.
	 * A different object with the same identifier value was already associated with the session : [testAllAttributesPersistent#<bizId>]
	 * @throws Exception
	 */
	@Test
	void testPartialSave() throws Exception {
		DomainException de = assertThrows(DomainException.class, () -> {
			AllAttributesPersistent test = Util.constructRandomInstance(u, m, aapd, 3);
			test = p.save(test);
			test.setAggregatedAssociation(Util.constructRandomInstance(u, m, aapd, 0));
			test.setAggregatedAssociation(p.save(test.getAggregatedAssociation()));
		});

		assertThat(de.getMessage(), is(notNullValue()));
	}

	/**
	 * This test should not throw as the result of the merge has been assigned back to its parent object.
	 * Then flush is called.
	 * @throws Exception
	 */
	@Test
	void testPartialMerge() throws Exception {
		AllAttributesPersistent test = Util.constructRandomInstance(u, m, aapd, 3);
		test = p.save(test);
		test.setAggregatedAssociation(Util.constructRandomInstance(u, m, aapd, 0));
		test.setAggregatedAssociation(p.merge(test.getAggregatedAssociation()));
		p.flush();
	}

	@Test
	void testPersistBizLockEJS() throws Exception {
		MappedExtensionJoinedStrategy test = Util.constructRandomInstance(u, m, mejsd, 3);
		assertNull(test.getBizLock());
		assertNull(test.getAggregatedAssociation().getBizLock());
		assertNull(test.getAggregatedCollection().get(0).getBizLock());

		test = p.save(test);

		assertNotNull(test.getBizLock());
		assertNotNull(test.getAggregatedAssociation().getBizLock());
		assertNotNull(test.getAggregatedCollection().get(0).getBizLock());
	}

	@Test
	void testPersistBizLockESS() throws Exception {
		MappedExtensionSingleStrategy test = Util.constructRandomInstance(u, m, messd, 3);
		assertNull(test.getBizLock());
		assertNull(test.getAggregatedAssociation().getBizLock());
		assertNull(test.getAggregatedCollection().get(0).getBizLock());

		test = p.save(test);

		assertNotNull(test.getBizLock());
		assertNotNull(test.getAggregatedAssociation().getBizLock());
		assertNotNull(test.getAggregatedCollection().get(0).getBizLock());
	}

	@Test
	void testPersistBizLockSJS() throws Exception {
		MappedSubclassedJoinedStrategy test = Util.constructRandomInstance(u, m, msjsd, 3);
		assertNull(test.getBizLock());
		assertNull(test.getAggregatedAssociation().getBizLock());
		assertNull(test.getAggregatedCollection().get(0).getBizLock());

		test = p.save(test);

		assertNotNull(test.getBizLock());
		assertNotNull(test.getAggregatedAssociation().getBizLock());
		assertNotNull(test.getAggregatedCollection().get(0).getBizLock());
	}

	@Test
	void testPersistBizLockSSS() throws Exception {
		MappedSubclassedSingleStrategy test = Util.constructRandomInstance(u, m, msssd, 3);
		assertNull(test.getBizLock());
		assertNull(test.getAggregatedAssociation().getBizLock());
		assertNull(test.getAggregatedCollection().get(0).getBizLock());

		test = p.save(test);

		assertNotNull(test.getBizLock());
		assertNotNull(test.getAggregatedAssociation().getBizLock());
		assertNotNull(test.getAggregatedCollection().get(0).getBizLock());
	}

	@Test
	void testPreAndPostSaveAndDeleteNoBaseESS() throws Exception {
		MappedExtensionSingleStrategyExtension test = Util.constructRandomInstance(u, m, messd, 3);

		assertFalse(test.isPreSaveCalled());
		assertFalse(test.isPostSaveCalled());
		assertFalse(test.getAggregatedAssociation().isPreSaveCalled());
		assertFalse(test.getAggregatedAssociation().isPostSaveCalled());
		assertFalse(test.getComposedAssociation().isPreSaveCalled());
		assertFalse(test.getComposedAssociation().isPostSaveCalled());
		assertFalse(test.getAggregatedCollection().get(0).isPreSaveCalled());
		assertFalse(test.getAggregatedCollection().get(0).isPostSaveCalled());
		assertFalse(test.getAggregatedCollection().get(1).isPreSaveCalled());
		assertFalse(test.getAggregatedCollection().get(1).isPostSaveCalled());

		assertFalse(test.isPreDeleteCalled());
		assertFalse(test.isPostDeleteCalled());
		assertFalse(test.getAggregatedAssociation().isPreDeleteCalled());
		assertFalse(test.getAggregatedAssociation().isPostDeleteCalled());
		assertFalse(test.getComposedAssociation().isPreDeleteCalled());
		assertFalse(test.getComposedAssociation().isPostDeleteCalled());
		assertFalse(test.getAggregatedCollection().get(0).isPreDeleteCalled());
		assertFalse(test.getAggregatedCollection().get(0).isPostDeleteCalled());
		assertFalse(test.getAggregatedCollection().get(1).isPreDeleteCalled());
		assertFalse(test.getAggregatedCollection().get(1).isPostDeleteCalled());

		// Don't assign this coz we wanna test the old transient bean for evidence of bizlet calls
		MappedExtensionSingleStrategyExtension persistedTest = p.save(test);

		assertTrue(test.isPreSaveCalled());
		assertTrue(test.getAggregatedAssociation().isPreSaveCalled());
		assertTrue(test.getComposedAssociation().isPreSaveCalled());
		assertTrue(test.getAggregatedCollection().get(0).isPreSaveCalled());
		assertTrue(test.getAggregatedCollection().get(1).isPreSaveCalled());

		// Post save is called on the now managed persistent version
		assertTrue(persistedTest.isPostSaveCalled());
		assertTrue(persistedTest.getAggregatedAssociation().isPostSaveCalled());
		assertTrue(persistedTest.getComposedAssociation().isPostSaveCalled());
		assertTrue(persistedTest.getAggregatedCollection().get(0).isPostSaveCalled());
		assertTrue(persistedTest.getAggregatedCollection().get(1).isPostSaveCalled());
		
		p.delete(persistedTest);
		
		assertTrue(persistedTest.isPreDeleteCalled());
		assertTrue(persistedTest.isPostDeleteCalled());
		assertFalse(persistedTest.getAggregatedAssociation().isPreDeleteCalled());
		assertFalse(persistedTest.getAggregatedAssociation().isPostDeleteCalled());
		assertTrue(persistedTest.getComposedAssociation().isPreDeleteCalled());
		assertTrue(persistedTest.getComposedAssociation().isPostDeleteCalled());
		assertFalse(persistedTest.getAggregatedCollection().get(0).isPreDeleteCalled());
		assertFalse(persistedTest.getAggregatedCollection().get(0).isPostDeleteCalled());
		assertFalse(persistedTest.getAggregatedCollection().get(1).isPreDeleteCalled());
		assertFalse(persistedTest.getAggregatedCollection().get(1).isPostDeleteCalled());
	}

	@Test
	void testPreAndPostSaveAndDeleteNoBaseEJS() throws Exception {
		MappedExtensionJoinedStrategyExtension test = Util.constructRandomInstance(u, m, mejsd, 3);

		assertFalse(test.isPreSaveCalled());
		assertFalse(test.isPostSaveCalled());
		assertFalse(test.getAggregatedAssociation().isPreSaveCalled());
		assertFalse(test.getAggregatedAssociation().isPostSaveCalled());
		assertFalse(test.getComposedAssociation().isPreSaveCalled());
		assertFalse(test.getComposedAssociation().isPostSaveCalled());
		assertFalse(test.getAggregatedCollection().get(0).isPreSaveCalled());
		assertFalse(test.getAggregatedCollection().get(0).isPostSaveCalled());
		assertFalse(test.getAggregatedCollection().get(1).isPreSaveCalled());
		assertFalse(test.getAggregatedCollection().get(1).isPostSaveCalled());

		assertFalse(test.isPreDeleteCalled());
		assertFalse(test.isPostDeleteCalled());
		assertFalse(test.getAggregatedAssociation().isPreDeleteCalled());
		assertFalse(test.getAggregatedAssociation().isPostDeleteCalled());
		assertFalse(test.getComposedAssociation().isPreDeleteCalled());
		assertFalse(test.getComposedAssociation().isPostDeleteCalled());
		assertFalse(test.getAggregatedCollection().get(0).isPreDeleteCalled());
		assertFalse(test.getAggregatedCollection().get(0).isPostDeleteCalled());
		assertFalse(test.getAggregatedCollection().get(1).isPreDeleteCalled());
		assertFalse(test.getAggregatedCollection().get(1).isPostDeleteCalled());

		// Don't assign this coz we wanna test the old transient bean for evidence of bizlet calls
		MappedExtensionJoinedStrategyExtension persistedTest = p.save(test);

		assertTrue(test.isPreSaveCalled());
		assertTrue(test.getAggregatedAssociation().isPreSaveCalled());
		assertTrue(test.getComposedAssociation().isPreSaveCalled());
		assertTrue(test.getAggregatedCollection().get(0).isPreSaveCalled());
		assertTrue(test.getAggregatedCollection().get(1).isPreSaveCalled());

		// Post save is called on the now managed persistent version
		assertTrue(persistedTest.isPostSaveCalled());
		assertTrue(persistedTest.getAggregatedAssociation().isPostSaveCalled());
		assertTrue(persistedTest.getComposedAssociation().isPostSaveCalled());
		assertTrue(persistedTest.getAggregatedCollection().get(0).isPostSaveCalled());
		assertTrue(persistedTest.getAggregatedCollection().get(1).isPostSaveCalled());
		
		p.delete(persistedTest);
		
		assertTrue(persistedTest.isPreDeleteCalled());
		assertTrue(persistedTest.isPostDeleteCalled());
		assertFalse(persistedTest.getAggregatedAssociation().isPreDeleteCalled());
		assertFalse(persistedTest.getAggregatedAssociation().isPostDeleteCalled());
		assertTrue(persistedTest.getComposedAssociation().isPreDeleteCalled());
		assertTrue(persistedTest.getComposedAssociation().isPostDeleteCalled());
		assertFalse(persistedTest.getAggregatedCollection().get(0).isPreDeleteCalled());
		assertFalse(persistedTest.getAggregatedCollection().get(0).isPostDeleteCalled());
		assertFalse(persistedTest.getAggregatedCollection().get(1).isPreDeleteCalled());
		assertFalse(persistedTest.getAggregatedCollection().get(1).isPostDeleteCalled());
	}

	@Test
	void testDeleteDuringPostDelete() throws Exception {
		Document dupdd = m.getDocument(c, DeleteDuringPostDelete.DOCUMENT_NAME);
		DeleteDuringPostDelete test = Util.constructRandomInstance(u, m, dupdd, 2);
		test = p.save(test);
		assertTrue(test.getAggregatedAssociation().isPersisted());
		p.delete(test);
	}
	
	@Test
	void testDerivedPropertiesAvailableAfterSaveESS() throws Exception {
		MappedExtensionSingleStrategyExtension test = Util.constructRandomInstance(u, m, messd, 4);
		testDerivedPropertiesAvailableAfterSave(test);
	}

	@Test
	void testDerivedPropertiesAvailableAfterSaveSSS() throws Exception {
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
			assertNotNull(value);
		}

		MappedExtensionSingleStrategyExtension persistedTest = p.save(test);

		assertEquals(values[0], persistedTest.getBaseDerivedInteger());
		assertEquals(values[1], persistedTest.getDerivedInteger());
		assertEquals(values[2], persistedTest.getAggregatedAssociation().getBaseDerivedInteger());
		assertEquals(values[3], persistedTest.getAggregatedAssociation().getDerivedInteger());
		assertEquals(values[4], persistedTest.getAggregatedCollection().get(0).getBaseDerivedInteger());
		assertEquals(values[5], persistedTest.getAggregatedCollection().get(0).getDerivedInteger());
		assertEquals(values[6], persistedTest.getAggregatedCollection().get(1).getBaseDerivedInteger());
		assertEquals(values[7], persistedTest.getAggregatedCollection().get(1).getDerivedInteger());
	}

	@Test
	void testDerivedPropertiesAvailableAfterSaveEJS() throws Exception {
		MappedExtensionJoinedStrategyExtension test = Util.constructRandomInstance(u, m, mejsd, 4);
		testDerivedPropertiesAvailableAfterSave(test);
	}

	@Test
	void testDerivedPropertiesAvailableAfterSaveSJS() throws Exception {
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
			assertNotNull(value);
		}

		MappedExtensionJoinedStrategyExtension persistedTest = p.save(test);

		assertEquals(values[0], persistedTest.getBaseDerivedInteger());
		assertEquals(values[1], persistedTest.getDerivedInteger());
		assertEquals(values[2], persistedTest.getAggregatedAssociation().getBaseDerivedInteger());
		assertEquals(values[3], persistedTest.getAggregatedAssociation().getDerivedInteger());
		assertEquals(values[4], persistedTest.getAggregatedCollection().get(0).getBaseDerivedInteger());
		assertEquals(values[5], persistedTest.getAggregatedCollection().get(0).getDerivedInteger());
		assertEquals(values[6], persistedTest.getAggregatedCollection().get(1).getBaseDerivedInteger());
		assertEquals(values[7], persistedTest.getAggregatedCollection().get(1).getDerivedInteger());
	}

	@Test
	void testAggregatedAssociationDoesntCascadeDelete() throws Exception {
		AllAttributesPersistent test = Util.constructRandomInstance(u, m, aapd, 2);
		test.setComposedAssociation(null);
		test.getComposedCollection().clear();
		test.getAggregatedCollection().clear();
		test = p.save(test);
		String associationBizId = test.getAggregatedAssociation().getBizId();
		assertEquals(2, p.newSQL("select count(1) from TEST_AllAttributesPersistent").scalarResult(Number.class).intValue());
		p.delete(test);
		assertEquals(associationBizId, p.newSQL("select bizId from TEST_AllAttributesPersistent").scalarResult(String.class));
	}

	@Test
	void testComposedAssociationDoesCascadeDelete() throws Exception {
		AllAttributesPersistent test = Util.constructRandomInstance(u, m, aapd, 2);
		test.setAggregatedAssociation(null);
		test.getAggregatedCollection().clear();
		test.getComposedCollection().clear();
		test = p.save(test);
		assertEquals(2, p.newSQL("select count(1) from TEST_AllAttributesPersistent").scalarResult(Number.class).intValue());
		p.delete(test);
		assertEquals(0, p.newSQL("select count(1) from TEST_AllAttributesPersistent").scalarResult(Number.class).intValue());
	}

	@Test
	void testComposedAssociationNulledDoesCascadeDelete() throws Exception {
		AllAttributesPersistent test = Util.constructRandomInstance(u, m, aapd, 2);
		test.setAggregatedAssociation(null);
		test.getAggregatedCollection().clear();
		test.getComposedCollection().clear();
		test = p.save(test);
		assertEquals(2, p.newSQL("select count(1) from TEST_AllAttributesPersistent").scalarResult(Number.class).intValue());
		test.setComposedAssociation(null);
		p.save(test);
		assertEquals(1, p.newSQL("select count(1) from TEST_AllAttributesPersistent").scalarResult(Number.class).intValue());
	}

	@Test
	void testComposedCascadeDeleteExtensionSingleStrategy() throws Exception {
		MappedExtensionSingleStrategy test = Util.constructRandomInstance(u, m, messd, 2);
		test.setAggregatedAssociation(null);
		test.getAggregatedCollection().clear();
		test = p.save(test);

		assertEquals(4,
				p.newSQL("select count(1) from TEST_MappedExtensionSingleStrategy").scalarResult(Number.class).intValue());
		assertEquals(2, p.newSQL("select count(1) from TEST_MappedExtensionSingleStrategy_composedCollection")
				.scalarResult(Number.class).intValue());

		p.delete(test);

		// There should be no TEST_MappedExtension rows in the database since they cascade
		assertEquals(0,
				p.newSQL("select count(1) from TEST_MappedExtensionSingleStrategy").scalarResult(Number.class).intValue());
		assertEquals(0, p.newSQL("select count(1) from TEST_MappedExtensionSingleStrategy_composedCollection")
				.scalarResult(Number.class).intValue());
	}

	@Test
	void testComposedCascadeDeleteSubclassedSingleStrategy() throws Exception {
		MappedSubclassedSingleStrategy test = Util.constructRandomInstance(u, m, msssd, 2);
		test.setAggregatedAssociation(null);
		test.getAggregatedCollection().clear();
		test = p.save(test);

		assertEquals(4,
				p.newSQL("select count(1) from TEST_MappedExtensionSingleStrategy").scalarResult(Number.class).intValue());
		assertEquals(2, p.newSQL("select count(1) from TEST_MappedExtensionSingleStrategy_composedCollection")
				.scalarResult(Number.class).intValue());

		p.delete(test);

		// There should be no TEST_MappedExtension rows in the database since they cascade
		assertEquals(0,
				p.newSQL("select count(1) from TEST_MappedExtensionSingleStrategy").scalarResult(Number.class).intValue());
		assertEquals(0, p.newSQL("select count(1) from TEST_MappedExtensionSingleStrategy_composedCollection")
				.scalarResult(Number.class).intValue());
	}

	@Test
	void testComposedCascadeDeleteExtensionJoinedStrategy() throws Exception {
		MappedExtensionJoinedStrategy test = Util.constructRandomInstance(u, m, mejsd, 2);
		test.setAggregatedAssociation(null);
		test.getAggregatedCollection().clear();
		test = p.save(test);

		assertEquals(3,
				p.newSQL("select count(1) from TEST_MappedExtensionJoinedStrategy").scalarResult(Number.class).intValue());
		assertEquals(2, p.newSQL("select count(1) from TEST_MappedExtensionJoinedStrategy_composedCollection")
				.scalarResult(Number.class).intValue());

		p.delete(test);

		// There should be no TEST_MappedExtension rows in the database since they cascade
		assertEquals(0,
				p.newSQL("select count(1) from TEST_MappedExtensionJoinedStrategy").scalarResult(Number.class).intValue());
		assertEquals(0, p.newSQL("select count(1) from TEST_MappedExtensionJoinedStrategy_composedCollection")
				.scalarResult(Number.class).intValue());
	}

	@Test
	void testComposedCascadeDeleteSubclassedJoinedStrategy() throws Exception {
		MappedSubclassedJoinedStrategy test = Util.constructRandomInstance(u, m, msjsd, 2);
		test.setAggregatedAssociation(null);
		test.getAggregatedCollection().clear();
		test = p.save(test);

		assertEquals(3,
				p.newSQL("select count(1) from TEST_MappedExtensionJoinedStrategy").scalarResult(Number.class).intValue());
		assertEquals(1,
				p.newSQL("select count(1) from TEST_MappedSubclassedJoinedStrategy").scalarResult(Number.class).intValue());
		assertEquals(2, p.newSQL("select count(1) from TEST_MappedExtensionJoinedStrategy_composedCollection")
				.scalarResult(Number.class).intValue());

		p.delete(test);

		// There should be no TEST_MappedExtension rows in the database since they cascade
		assertEquals(0,
				p.newSQL("select count(1) from TEST_MappedExtensionJoinedStrategy").scalarResult(Number.class).intValue());
		assertEquals(0,
				p.newSQL("select count(1) from TEST_MappedSubclassedJoinedStrategy").scalarResult(Number.class).intValue());
		assertEquals(0, p.newSQL("select count(1) from TEST_MappedExtensionJoinedStrategy_composedCollection")
				.scalarResult(Number.class).intValue());
	}

	@Test
	void testAggregatedCascadeDeleteExtensionSingleStrategy() throws Exception {
		MappedExtensionSingleStrategy test = Util.constructRandomInstance(u, m, messd, 2);
		test = p.save(test);

		assertEquals(7,
				p.newSQL("select count(1) from TEST_MappedExtensionSingleStrategy").scalarResult(Number.class).intValue());
		assertEquals(2, p.newSQL("select count(1) from TEST_MappedExtensionSingleStrategy_composedCollection")
				.scalarResult(Number.class).intValue());
		assertEquals(2, p.newSQL("select count(1) from TEST_MappedExtensionSingleStrategy_aggregatedCollection")
				.scalarResult(Number.class).intValue());

		p.delete(test);

		// There should be 3 TEST_MappedExtension rows in the database since they cascade
		// but the aggregated ones (2 in collections and 1 associations) are left behind
		assertEquals(3,
				p.newSQL("select count(1) from TEST_MappedExtensionSingleStrategy").scalarResult(Number.class).intValue());
		assertEquals(0, p.newSQL("select count(1) from TEST_MappedExtensionSingleStrategy_composedCollection")
				.scalarResult(Number.class).intValue());
		assertEquals(0, p.newSQL("select count(1) from TEST_MappedExtensionSingleStrategy_aggregatedCollection")
				.scalarResult(Number.class).intValue());
	}

	@Test
	void testAggregatedCascadeDeleteSubclassedSingleStrategy() throws Exception {
		MappedSubclassedSingleStrategy test = Util.constructRandomInstance(u, m, msssd, 2);
		test = p.save(test);

		assertEquals(7,
				p.newSQL("select count(1) from TEST_MappedExtensionSingleStrategy").scalarResult(Number.class).intValue());
		assertEquals(2, p.newSQL("select count(1) from TEST_MappedExtensionSingleStrategy_composedCollection")
				.scalarResult(Number.class).intValue());
		assertEquals(2, p.newSQL("select count(1) from TEST_MappedExtensionSingleStrategy_aggregatedCollection")
				.scalarResult(Number.class).intValue());

		p.delete(test);

		// There should be 3 TEST_MappedExtension rows in the database since they cascade
		// but the aggregated ones (2 in collections and 1 associations) are left behind
		assertEquals(3,
				p.newSQL("select count(1) from TEST_MappedExtensionSingleStrategy").scalarResult(Number.class).intValue());
		assertEquals(0, p.newSQL("select count(1) from TEST_MappedExtensionSingleStrategy_composedCollection")
				.scalarResult(Number.class).intValue());
		assertEquals(0, p.newSQL("select count(1) from TEST_MappedExtensionSingleStrategy_aggregatedCollection")
				.scalarResult(Number.class).intValue());
	}

	@Test
	void testAggregatedCascadeDeleteExtensionJoinedStrategy() throws Exception {
		MappedExtensionJoinedStrategy test = Util.constructRandomInstance(u, m, mejsd, 2);
		test = p.save(test);

		assertEquals(6,
				p.newSQL("select count(1) from TEST_MappedExtensionJoinedStrategy").scalarResult(Number.class).intValue());
		assertEquals(2, p.newSQL("select count(1) from TEST_MappedExtensionJoinedStrategy_composedCollection")
				.scalarResult(Number.class).intValue());
		assertEquals(2, p.newSQL("select count(1) from TEST_MappedExtensionJoinedStrategy_aggregatedCollection")
				.scalarResult(Number.class).intValue());

		p.delete(test);

		// There should be 3 TEST_MappedExtension rows in the database since they cascade
		// but the aggregated ones (2 in collections and 1 associations) are left behind
		assertEquals(3,
				p.newSQL("select count(1) from TEST_MappedExtensionJoinedStrategy").scalarResult(Number.class).intValue());
		assertEquals(0, p.newSQL("select count(1) from TEST_MappedExtensionJoinedStrategy_composedCollection")
				.scalarResult(Number.class).intValue());
		assertEquals(0, p.newSQL("select count(1) from TEST_MappedExtensionJoinedStrategy_aggregatedCollection")
				.scalarResult(Number.class).intValue());
	}

	@Test
	void testAggregatedCascadeDeleteSubclassedJoinedStrategy() throws Exception {
		MappedSubclassedJoinedStrategy test = Util.constructRandomInstance(u, m, msjsd, 2);
		test = p.save(test);

		assertEquals(6,
				p.newSQL("select count(1) from TEST_MappedExtensionJoinedStrategy").scalarResult(Number.class).intValue());
		assertEquals(1,
				p.newSQL("select count(1) from TEST_MappedSubclassedJoinedStrategy").scalarResult(Number.class).intValue());
		assertEquals(2, p.newSQL("select count(1) from TEST_MappedExtensionJoinedStrategy_composedCollection")
				.scalarResult(Number.class).intValue());
		assertEquals(2, p.newSQL("select count(1) from TEST_MappedExtensionJoinedStrategy_aggregatedCollection")
				.scalarResult(Number.class).intValue());

		p.delete(test);

		// There should be 3 TEST_MappedExtension rows in the database since they cascade
		// but the aggregated ones (2 in collections and 1 associations) are left behind
		assertEquals(3,
				p.newSQL("select count(1) from TEST_MappedExtensionJoinedStrategy").scalarResult(Number.class).intValue());
		assertEquals(0,
				p.newSQL("select count(1) from TEST_MappedSubclassedJoinedStrategy").scalarResult(Number.class).intValue());
		assertEquals(0, p.newSQL("select count(1) from TEST_MappedExtensionJoinedStrategy_composedCollection")
				.scalarResult(Number.class).intValue());
		assertEquals(0, p.newSQL("select count(1) from TEST_MappedExtensionJoinedStrategy_aggregatedCollection")
				.scalarResult(Number.class).intValue());
	}

	@Test
	void testAggregatedAssociationReferentialIntegritySingleStrategy() throws Exception {
		ReferentialConstraintViolationException rcve = assertThrows(ReferentialConstraintViolationException.class, () -> {
			MappedExtensionSingleStrategy test = Util.constructRandomInstance(u, m, messd, 2);
			test = p.save(test);

			p.delete(test.getAggregatedAssociation());
		});

		assertThat(rcve.getMessage(), is(notNullValue()));
	}

	@Test
	void testAggregatedCollectionReferentialIntegritySingleStrategy() throws Exception {
		ReferentialConstraintViolationException rcve = assertThrows(ReferentialConstraintViolationException.class, () -> {
			MappedExtensionSingleStrategy test = Util.constructRandomInstance(u, m, messd, 2);
			test = p.save(test);

			p.delete(test.getAggregatedCollection().get(0));
		});

		assertThat(rcve.getMessage(), is(notNullValue()));
	}

	@Test
	void testComposedAssociationReferentialIntegritySingleStrategy() throws Exception {
		ReferentialConstraintViolationException rcve = assertThrows(ReferentialConstraintViolationException.class, () -> {
			MappedExtensionSingleStrategy test = Util.constructRandomInstance(u, m, messd, 2);
			test = p.save(test);

			p.delete(test.getComposedAssociation());
		});

		assertThat(rcve.getMessage(), is(notNullValue()));
	}

	@Test
	void testComposedCollectionReferentialIntegritySingleStrategy() throws Exception {
		ReferentialConstraintViolationException rcve = assertThrows(ReferentialConstraintViolationException.class, () -> {
			MappedExtensionSingleStrategy test = Util.constructRandomInstance(u, m, messd, 2);
			test = p.save(test);

			p.delete(test.getComposedCollection().get(0));
		});

		assertThat(rcve.getMessage(), is(notNullValue()));
	}

	@Test
	@SuppressWarnings("java:S1854") // Persistence.save() may return a different instance; assignment captured intentionally
	void testComposedCollectionRemoveMemberSingleStrategy() throws Exception {
		MappedExtensionSingleStrategy test = Util.constructRandomInstance(u, m, messd, 2);
		test = p.save(test);

		assertEquals(7,
				p.newSQL("select count(1) from TEST_MappedExtensionSingleStrategy").scalarResult(Number.class).intValue());
		assertEquals(2, p.newSQL("select count(1) from TEST_MappedExtensionSingleStrategy_composedCollection")
				.scalarResult(Number.class).intValue());
		assertEquals(2, p.newSQL("select count(1) from TEST_MappedExtensionSingleStrategy_aggregatedCollection")
				.scalarResult(Number.class).intValue());

		test.getComposedCollection().remove(0);

		test = p.save(test);

		// Check the composed collection element got cascaded with no referential integrity troubles
		assertEquals(6,
				p.newSQL("select count(1) from TEST_MappedExtensionSingleStrategy").scalarResult(Number.class).intValue());
		assertEquals(1, p.newSQL("select count(1) from TEST_MappedExtensionSingleStrategy_composedCollection")
				.scalarResult(Number.class).intValue());
		assertEquals(2, p.newSQL("select count(1) from TEST_MappedExtensionSingleStrategy_aggregatedCollection")
				.scalarResult(Number.class).intValue());
	}

	@Test
	void testComposedCollectionMoveMemberSingleStrategy() throws Exception {
		MappedExtensionSingleStrategy source = Util.constructRandomInstance(u, m, messd, 2);
		source = p.save(source);

		assertEquals(7,
				p.newSQL("select count(1) from TEST_MappedExtensionSingleStrategy").scalarResult(Number.class).intValue());
		assertEquals(2, p.newSQL("select count(1) from TEST_MappedExtensionSingleStrategy_composedCollection")
				.scalarResult(Number.class).intValue());
		assertEquals(2, p.newSQL("select count(1) from TEST_MappedExtensionSingleStrategy_aggregatedCollection")
				.scalarResult(Number.class).intValue());

		MappedExtensionSingleStrategy dest = Util.constructRandomInstance(u, m, messd, 2);
		dest = p.save(dest);

		assertEquals(14,
				p.newSQL("select count(1) from TEST_MappedExtensionSingleStrategy").scalarResult(Number.class).intValue());
		assertEquals(4, p.newSQL("select count(1) from TEST_MappedExtensionSingleStrategy_composedCollection")
				.scalarResult(Number.class).intValue());
		assertEquals(4, p.newSQL("select count(1) from TEST_MappedExtensionSingleStrategy_aggregatedCollection")
				.scalarResult(Number.class).intValue());

		MappedExtensionSingleStrategyExtension element = source.getComposedCollection().remove(0);
		source = p.save(source);
		dest.getComposedCollection().add(element);
		dest = p.save(dest);
		
		// Check the composed collection element got cascaded with no referential integrity troubles
		assertEquals(14,
				p.newSQL("select count(1) from TEST_MappedExtensionSingleStrategy").scalarResult(Number.class).intValue());
		assertEquals(1, p.newSQL("select count(1) from TEST_MappedExtensionSingleStrategy_composedCollection where owner_id = :id")
								.putParameter("id", source.getBizId(), false)
								.scalarResult(Number.class).intValue());
		assertEquals(3, p.newSQL("select count(1) from TEST_MappedExtensionSingleStrategy_composedCollection where owner_id = :id")
								.putParameter("id", dest.getBizId(), false)
								.scalarResult(Number.class).intValue());
		assertEquals(4, p.newSQL("select count(1) from TEST_MappedExtensionSingleStrategy_aggregatedCollection")
								.scalarResult(Number.class).intValue());
	}

	@Test
	void testComposedCollectionMoveMemberWithoutFlushThrows() throws Exception {
		OptimisticLockException ole = assertThrows(OptimisticLockException.class, () -> {
			MappedExtensionSingleStrategy source = Util.constructRandomInstance(u, m, messd, 2);
			source = p.save(source);
			MappedExtensionSingleStrategy dest = Util.constructRandomInstance(u, m, messd, 2);
			dest = p.save(dest);

			MappedExtensionSingleStrategyExtension element = source.getComposedCollection().remove(0);
			dest.getComposedCollection().add(element);
			p.save(source, dest);
		});

		assertThat(ole.getMessage(), is(notNullValue()));
	}

	@Test
	void testAggregatedAssociationReferentialIntegrityJoinedStrategy() throws Exception {
		ReferentialConstraintViolationException rcve = assertThrows(ReferentialConstraintViolationException.class, () -> {
			MappedExtensionJoinedStrategy test = Util.constructRandomInstance(u, m, mejsd, 2);
			test = p.save(test);

			p.delete(test.getAggregatedAssociation());
		});

		assertThat(rcve.getMessage(), is(notNullValue()));
	}

	@Test
	void testAggregatedCollectionReferentialIntegrityJoinedStrategy() throws Exception {
		ReferentialConstraintViolationException rcve = assertThrows(ReferentialConstraintViolationException.class, () -> {
			MappedExtensionJoinedStrategy test = Util.constructRandomInstance(u, m, mejsd, 2);
			test = p.save(test);

			p.delete(test.getAggregatedCollection().get(0));
		});

		assertThat(rcve.getMessage(), is(notNullValue()));
	}

	@Test
	void testComposedAssociationReferentialIntegrityJoinedStrategy() throws Exception {
		ReferentialConstraintViolationException rcve = assertThrows(ReferentialConstraintViolationException.class, () -> {
			MappedExtensionJoinedStrategy test = Util.constructRandomInstance(u, m, mejsd, 2);
			test = p.save(test);

			p.delete(test.getComposedAssociation());
		});

		assertThat(rcve.getMessage(), is(notNullValue()));
	}

	@Test
	void testComposedCollectionReferentialIntegrityJoinedStrategy() throws Exception {
		ReferentialConstraintViolationException rcve = assertThrows(ReferentialConstraintViolationException.class, () -> {
			MappedExtensionJoinedStrategy test = Util.constructRandomInstance(u, m, mejsd, 2);
			test = p.save(test);

			p.delete(test.getComposedCollection().get(0));
		});

		assertThat(rcve.getMessage(), is(notNullValue()));
	}

	@Test
	@SuppressWarnings("java:S1854") // Persistence.save() may return a different instance; assignment captured intentionally
	void testComposedCollectionRemoveMemberJoinedStrategy() throws Exception {
		MappedExtensionJoinedStrategy test = Util.constructRandomInstance(u, m, mejsd, 2);
		test = p.save(test);

		assertEquals(6,
				p.newSQL("select count(1) from TEST_MappedExtensionJoinedStrategy").scalarResult(Number.class).intValue());
		assertEquals(0,
				p.newSQL("select count(1) from TEST_MappedSubclassedJoinedStrategy").scalarResult(Number.class).intValue());
		assertEquals(2, p.newSQL("select count(1) from TEST_MappedExtensionJoinedStrategy_composedCollection")
				.scalarResult(Number.class).intValue());
		assertEquals(2, p.newSQL("select count(1) from TEST_MappedExtensionJoinedStrategy_aggregatedCollection")
				.scalarResult(Number.class).intValue());

		test.getComposedCollection().remove(0);

		test = p.save(test);

		// Check the composed collection element got cascaded with no referential integrity troubles
		assertEquals(5,
				p.newSQL("select count(1) from TEST_MappedExtensionJoinedStrategy").scalarResult(Number.class).intValue());
		assertEquals(0,
				p.newSQL("select count(1) from TEST_MappedSubclassedJoinedStrategy").scalarResult(Number.class).intValue());
		assertEquals(1, p.newSQL("select count(1) from TEST_MappedExtensionJoinedStrategy_composedCollection")
				.scalarResult(Number.class).intValue());
		assertEquals(2, p.newSQL("select count(1) from TEST_MappedExtensionJoinedStrategy_aggregatedCollection")
				.scalarResult(Number.class).intValue());

	}

	@Test
	void testComposedCollectionMoveMemberJoinedStrategy() throws Exception {
		MappedExtensionJoinedStrategy source = Util.constructRandomInstance(u, m, mejsd, 2);
		source = p.save(source);

		assertEquals(6,
				p.newSQL("select count(1) from TEST_MappedExtensionJoinedStrategy").scalarResult(Number.class).intValue());
		assertEquals(2, p.newSQL("select count(1) from TEST_MappedExtensionJoinedStrategy_composedCollection")
				.scalarResult(Number.class).intValue());
		assertEquals(2, p.newSQL("select count(1) from TEST_MappedExtensionJoinedStrategy_aggregatedCollection")
				.scalarResult(Number.class).intValue());

		MappedExtensionJoinedStrategy dest = Util.constructRandomInstance(u, m, mejsd, 2);
		dest = p.save(dest);

		assertEquals(12,
				p.newSQL("select count(1) from TEST_MappedExtensionJoinedStrategy").scalarResult(Number.class).intValue());
		assertEquals(4, p.newSQL("select count(1) from TEST_MappedExtensionJoinedStrategy_composedCollection")
				.scalarResult(Number.class).intValue());
		assertEquals(4, p.newSQL("select count(1) from TEST_MappedExtensionJoinedStrategy_aggregatedCollection")
				.scalarResult(Number.class).intValue());

		MappedExtensionJoinedStrategyExtension element = source.getComposedCollection().remove(0);
		source = p.save(source);
		dest.getComposedCollection().add(element);
		dest = p.save(dest);
		
		// Check the composed collection element got cascaded with no referential integrity troubles
		assertEquals(12,
				p.newSQL("select count(1) from TEST_MappedExtensionJoinedStrategy").scalarResult(Number.class).intValue());
		assertEquals(1, p.newSQL("select count(1) from TEST_MappedExtensionJoinedStrategy_composedCollection where owner_id = :id")
								.putParameter("id", source.getBizId(), false)
								.scalarResult(Number.class).intValue());
		assertEquals(3, p.newSQL("select count(1) from TEST_MappedExtensionJoinedStrategy_composedCollection where owner_id = :id")
								.putParameter("id", dest.getBizId(), false)
								.scalarResult(Number.class).intValue());
		assertEquals(4, p.newSQL("select count(1) from TEST_MappedExtensionJoinedStrategy_aggregatedCollection")
								.scalarResult(Number.class).intValue());
	}

	@Test
	void testOptimisticLockException() throws Exception {
		OptimisticLockException ole = assertThrows(OptimisticLockException.class, () -> {
			AllAttributesPersistent test = Util.constructRandomInstance(u, m, aapd, 1);
			test = p.save(test);

			String persistentIdentifier = aapd.getPersistent().getPersistentIdentifier();
			SQL sql = p.newSQL(String.format("update %s set %s = :%s, %s = :%s",
					persistentIdentifier,
					PersistentBean.LOCK_NAME,
					PersistentBean.LOCK_NAME,
					PersistentBean.VERSION_NAME,
					PersistentBean.VERSION_NAME));
			sql.putParameter(PersistentBean.LOCK_NAME, new OptimisticLock(u.getName(), new Date()).toString(), false);
			sql.putParameter(PersistentBean.VERSION_NAME, Integer.valueOf(2));
			sql.execute();

			test.setText("optimistic lock test");
			p.save(test);
		});

		assertThat(ole.getMessage(), is(notNullValue()));
	}

	@Test
	void testTransientStaleObjectStateExceptionOptimisticLock() throws Exception {
		OptimisticLockException ole = assertThrows(OptimisticLockException.class, () -> {
			AllAttributesPersistent test = Util.constructRandomInstance(u, m, aapd, 1);
			p.save(test); // NB not returned
			p.save(test);
		});

		assertThat(ole.getMessage(), is(notNullValue()));
	}

	@Test
	void testDetachedStaleObjectStateExceptionOptimisticLock() throws Exception {
		OptimisticLockException ole = assertThrows(OptimisticLockException.class, () -> {
			AllAttributesPersistent test = Util.constructRandomInstance(u, m, aapd, 1);
			test = p.save(test);

			p.evictCached(test);

			test.setText("optimistic lock test");
			p.save(test); // NB not returned

			test.setText("optimistic lock test take 2");
			p.save(test);
		});

		assertThat(ole.getMessage(), is(notNullValue()));
	}

	@Test
	void testClonedStaleObjectStateExceptionOptimisticLock() throws Exception {
		OptimisticLockException ole = assertThrows(OptimisticLockException.class, () -> {
			AllAttributesPersistent test = Util.constructRandomInstance(u, m, aapd, 1);
			test = p.save(test);

			test = Util.cloneToTransientBySerialization(test);

			test.setText("optimistic lock test");
			p.save(test); // NB not returned

			test.setText("optimistic lock test take 2");
			p.save(test);
		});

		assertThat(ole.getMessage(), is(notNullValue()));
	}

	@Test
	void testRefresh() throws Exception {
		AllAttributesPersistent test = Util.constructRandomInstance(u, m, aapd, 1);
		test = p.save(test);
		test.setText("optimistic lock test");
		((AbstractHibernatePersistence) p).refresh(test);
		assertNotEquals("optimistic lock test", test.getText());
	}

	@Test
	void testRefreshTransient() throws Exception {
		AllAttributesPersistent test = Util.constructRandomInstance(u, m, aapd, 1);
		((AbstractHibernatePersistence) p).refresh(test);
	}

	@Test
	void testRefreshDetached() throws Exception {
		DomainException de = assertThrows(DomainException.class, () -> {
			AllAttributesPersistent test = Util.constructRandomInstance(u, m, aapd, 1);
			test = p.save(test);
			p.evictCached(test);
			((AbstractHibernatePersistence) p).refresh(test);
		});

		assertThat(de.getMessage(), is(notNullValue()));
	}

	@Test
	void testEvict() throws Exception {
		AllAttributesPersistent test = Util.constructRandomInstance(u, m, aapd, 1);
		@SuppressWarnings("resource")
		Session s = ((AbstractHibernatePersistence) p).getSession();
		assertFalse(s.contains(test.getBizModule() + test.getBizDocument(), test));
		test = p.save(test);
		assertTrue(s.contains(test.getBizModule() + test.getBizDocument(), test));
		p.evictCached(test);
		assertFalse(s.contains(test.getBizModule() + test.getBizDocument(), test));
	}
	
	@Test
	void testEvictEvicted() throws Exception {
		AllAttributesPersistent test = Util.constructRandomInstance(u, m, aapd, 1);
		@SuppressWarnings("resource")
		Session s = ((AbstractHibernatePersistence) p).getSession();
		assertFalse(s.contains(test.getBizModule() + test.getBizDocument(), test));
		test = p.save(test);
		assertTrue(s.contains(test.getBizModule() + test.getBizDocument(), test));
		p.evictCached(test);
		assertFalse(s.contains(test.getBizModule() + test.getBizDocument(), test));
		p.evictCached(test);
		assertFalse(s.contains(test.getBizModule() + test.getBizDocument(), test));
	}

	@Test
	void testEvictTransient() throws Exception {
		AllAttributesPersistent test = Util.constructRandomInstance(u, m, aapd, 1);
		@SuppressWarnings("resource")
		Session s = ((AbstractHibernatePersistence) p).getSession();
		assertFalse(s.contains(test.getBizModule() + test.getBizDocument(), test));
		p.evictCached(test);
		assertFalse(s.contains(test.getBizModule() + test.getBizDocument(), test));
	}
	
	@Test
	void testSaveOfList() throws Exception {
		AllAttributesPersistent test1 = Util.constructRandomInstance(u, m, aapd, 1);
		AllAttributesPersistent test2 = Util.constructRandomInstance(u, m, aapd, 1);

		List<AllAttributesPersistent> tests = new ArrayList<>();
		tests.add(test1);
		tests.add(test2);
		tests = p.save(tests);
		assertEquals(2, tests.size());
		assertEquals(test1.getBizId(), tests.get(0).getBizId());
		assertEquals(test2.getBizId(), tests.get(1).getBizId());
		assertEquals(2, p.newSQL("select count(1) from TEST_AllAttributesPersistent").scalarResult(Number.class).intValue());
		for (AllAttributesPersistent test : tests) {
			assertTrue(test.isPersisted());
		}
	}

	@Test
	void testSaveOfVarargs() throws Exception {
		AllAttributesPersistent test1 = Util.constructRandomInstance(u, m, aapd, 1);
		AllAttributesPersistent test2 = Util.constructRandomInstance(u, m, aapd, 1);

		List<AllAttributesPersistent> tests = p.save(test1, test2);
		assertEquals(2, tests.size());
		assertEquals(test1.getBizId(), tests.get(0).getBizId());
		assertEquals(test2.getBizId(), tests.get(1).getBizId());
		assertEquals(2, p.newSQL("select count(1) from TEST_AllAttributesPersistent").scalarResult(Number.class).intValue());
		for (AllAttributesPersistent test : tests) {
			assertTrue(test.isPersisted());
		}
	}

	
	@Test
	void testEmbeddedAssociationParent() throws Exception {
		AllAttributesPersistent test = Util.constructRandomInstance(u, m, aapd, 2);
		test = p.save(test);
		p.evictAllCached();
		test = p.retrieve(AllAttributesPersistent.MODULE_NAME, AllAttributesPersistent.DOCUMENT_NAME, test.getBizId());
		assertNotNull(test);
		assertEquals(test, test.getEmbeddedAssociation().getParent());
	}

	@Test
	void testNullInsertedEmbeddedAssociation() throws Exception {
		AllAttributesPersistent test = Util.constructRandomInstance(u, m, aapd, 1);
		test = p.save(test);
		p.evictAllCached();
		test = p.retrieve(AllAttributesPersistent.MODULE_NAME, AllAttributesPersistent.DOCUMENT_NAME, test.getBizId());
		assertNotNull(test);
		assertNull(test.getEmbeddedAssociation());
	}

	@Test
	void testNullUpdatedEmbeddedAssociation() throws Exception {
		AllAttributesPersistent test = Util.constructRandomInstance(u, m, aapd, 2);
		test = p.save(test);

		p.evictAllCached();
		test = p.retrieve(AllAttributesPersistent.MODULE_NAME, AllAttributesPersistent.DOCUMENT_NAME, test.getBizId());
		assertNotNull(test);
		assertNotNull(test.getEmbeddedAssociation());

		test.setEmbeddedAssociation(null);
		test = p.save(test);
		
		p.evictAllCached();
		test = p.retrieve(AllAttributesPersistent.MODULE_NAME, AllAttributesPersistent.DOCUMENT_NAME, test.getBizId());
		assertNotNull(test);
		assertNull(test.getEmbeddedAssociation());
	}
	
	@Test
	void testGeometry() throws Exception {
		AllAttributesPersistent test = Util.constructRandomInstance(u, m, aapd, 2);
		p.save(test);

		DocumentQuery q = p.newDocumentQuery(AllAttributesPersistent.MODULE_NAME, AllAttributesPersistent.DOCUMENT_NAME);
		Polygon poly = new GeometryFactory().createPolygon(new Coordinate[] {new Coordinate(-180, -360), new Coordinate(-180, 360), new Coordinate(180, 360), new Coordinate(180, -360), new Coordinate(-180, -360)});
		q.getFilter().addContains(AllAttributesPersistent.geometryPropertyName, poly);
		q.beanResults();
	}

	@Test
	void testSQLDyna() throws Exception {
		String persistentIdentifier = aapd.getPersistent().getPersistentIdentifier();
		p.newSQL("delete from " + persistentIdentifier).execute();
		
		AllAttributesPersistent aap = Util.constructRandomInstance(u, m, aapd, 1);
		aap = p.save(aap);
		aap = Util.constructRandomInstance(u, m, aapd, 1);
		aap = p.save(aap);
		aap = Util.constructRandomInstance(u, m, aapd, 1);
		aap = p.save(aap);

		DynaBean bean = p.newSQL("select bizId from " + persistentIdentifier).dynaResult();
			assertNotNull(bean, "bean should not be null");
			assertNotNull(Binder.get(bean, Bean.DOCUMENT_ID.toLowerCase()), "bizId should not be null");

			List<DynaBean> beans = p.newSQL("select bizId from " + persistentIdentifier).dynaResults();
			assertNotEquals(0, beans.size(), "Requires some data");
			for (DynaBean thisBean : beans) {
				assertNotNull(Binder.get(thisBean, Bean.DOCUMENT_ID.toLowerCase()), "bizId should not be null");
			}

			try (AutoClosingIterable<DynaBean> i = p.newSQL("select bizId from " + persistentIdentifier).dynaIterable()) {
				boolean exists = false;
				for (DynaBean thisBean : i) {
					assertNotNull(Binder.get(thisBean, Bean.DOCUMENT_ID.toLowerCase()), "bizId should not be null");
					exists = true;
				}
				assertTrue(exists, "Requires some data");
			}

			beans = p.newSQL("select bizId from " + persistentIdentifier + " where bizId = :bizId").putParameter(Bean.DOCUMENT_ID, "test", false).dynaResults();
			assertEquals(0, beans.size(), "Should be no matches");
	}

	@Test
	void testUpsertInsert() throws Exception {
		AllAttributesPersistent test = Util.constructRandomInstance(u, m, aapd, 2);
		test.setBizFlagComment("Something");

		p.upsertBeanTuple(test.getComposedAssociation());
		p.upsertBeanTuple(test.getAggregatedAssociation());
		p.upsertBeanTuple(test);
	}

	@Test
	void testUpsertUpdate() throws Exception {
		AllAttributesPersistent test = Util.constructRandomInstance(u, m, aapd, 2);
		test = p.save(test);
		test.setBizFlagComment("Something");
		p.upsertBeanTuple(test);
	}
	
	// Test an unpersisted bean assigned to a non-persistent aggregated association is not persisted by reachability
	@Test
	void testPersistThroughTransientAggregatedAssociation() throws Exception {
		Reachability test = rd.newInstance(u);
		test.setText("Text");
		AllAttributesPersistent a = aapd.newInstance(u);
		a.setText("Association");
		test.setNonPersistentAggregatedAssociation(a);
		test = p.save(test);
		
		assertFalse(a.isPersisted());
		assertFalse(test.getNonPersistentAggregatedAssociation().isPersisted());
	}

	// Test an unpersisted bean assigned to a non-persistent composed association is not persisted by reachability
	@Test
	void testPersistThroughTransientComposedAssociation() throws Exception {
		Reachability test = rd.newInstance(u);
		test.setText("Text");
		AllAttributesPersistent a = aapd.newInstance(u);
		a.setText("Association");
		test.setNonPersistentComposedAssociation(a);
		test = p.save(test);
		
		assertFalse(a.isPersisted());
		assertFalse(test.getNonPersistentComposedAssociation().isPersisted());
	}
	
	// Test an unpersisted bean assigned to a persistent aggregated association is persisted by reachability
	@Test
	void testPersistThroughPersistentAggregatedAssociation() throws Exception {
		Reachability test = rd.newInstance(u);
		test.setText("Text");
		AllAttributesPersistent a = aapd.newInstance(u);
		a.setText("Association");
		test.setPersistentAggregatedAssociation(a);
		test = p.save(test);
		
		assertFalse(a.isPersisted());
		assertTrue(test.getPersistentAggregatedAssociation().isPersisted());
	}

	// Test an unpersisted bean assigned to a persistent composed association is persisted by reachability
	@Test
	void testPersistThroughPersistentComposedAssociation() throws Exception {
		Reachability test = rd.newInstance(u);
		test.setText("Text");
		AllAttributesPersistent a = aapd.newInstance(u);
		a.setText("Association");
		test.setPersistentComposedAssociation(a);
		test = p.save(test);
		
		assertFalse(a.isPersisted());
		assertTrue(test.getPersistentComposedAssociation().isPersisted());
	}

	// Test an unpersisted bean assigned to a non-persistent aggregated collection is not persisted by reachability
	@Test
	void testPersistThroughTransientAggregatedCollection() throws Exception {
		Reachability test = rd.newInstance(u);
		test.setText("Text");
		AllAttributesPersistent a = aapd.newInstance(u);
		a.setText("Collection");
		test.getNonPersistentAggregatedCollection().add(a);
		test = p.save(test);
		
		assertFalse(a.isPersisted());
		assertFalse(test.getNonPersistentAggregatedCollection().get(0).isPersisted());
	}

	// Test an unpersisted bean assigned to a non-persistent composed collection is not persisted by reachability
	@Test
	void testPersistThroughTransientComposedCollection() throws Exception {
		Reachability test = rd.newInstance(u);
		test.setText("Text");
		AllAttributesPersistent a = aapd.newInstance(u);
		a.setText("Collection");
		test.getNonPersistentComposedCollection().add(a);
		test = p.save(test);
		
		assertFalse(a.isPersisted());
		assertFalse(test.getNonPersistentComposedCollection().get(0).isPersisted());
	}
	
	// Test an unpersisted bean assigned to a persistent aggregated collection is persisted by reachability
	@Test
	void testPersistThroughPersistentAggregatedCollection() throws Exception {
		Reachability test = rd.newInstance(u);
		test.setText("Text");
		AllAttributesPersistent a = aapd.newInstance(u);
		a.setText("Collection");
		test.getPersistentAggregatedCollection().add(a);
		test = p.save(test);
		
		assertFalse(a.isPersisted());
		assertTrue(test.getPersistentAggregatedCollection().get(0).isPersisted());
	}

	// Test an unpersisted bean assigned to a persistent composed collection is persisted by reachability
	@Test
	void testPersistThroughPersistentComposedCollection() throws Exception {
		Reachability test = rd.newInstance(u);
		test.setText("Text");
		AllAttributesPersistent a = aapd.newInstance(u);
		a.setText("Collection");
		test.getPersistentComposedCollection().add(a);
		test = p.save(test);
		
		assertFalse(a.isPersisted());
		assertTrue(test.getPersistentComposedCollection().get(0).isPersisted());
	}

	@Test
	void testSQLBeanResults() throws Exception {
		AllAttributesPersistent saved = Util.constructRandomInstance(u, m, aapd, 1);
		saved = p.save(saved);
		String table = aapd.getPersistent().getPersistentIdentifier();
		List<AllAttributesPersistent> results = p.newSQL(aapd, "select * from " + table + " where bizId = :id")
				.putParameter("id", saved.getBizId(), false)
				.beanResults();
		assertEquals(1, results.size());
	}

	@Test
	void testSQLBeanIterable() throws Exception {
		AllAttributesPersistent saved = Util.constructRandomInstance(u, m, aapd, 1);
		saved = p.save(saved);
		String table = aapd.getPersistent().getPersistentIdentifier();
		int count = 0;
		try (AutoClosingIterable<AllAttributesPersistent> i = p.newSQL(aapd, "select * from " + table + " where bizId = :id")
				.putParameter("id", saved.getBizId(), false)
				.beanIterable()) {
			for (@SuppressWarnings("unused") AllAttributesPersistent bean : i) {
				count++;
			}
		}
		assertEquals(1, count);
	}

	@Test
	void testSQLScalarIterable() throws Exception {
		AllAttributesPersistent saved = Util.constructRandomInstance(u, m, aapd, 1);
		p.save(saved);
		String table = aapd.getPersistent().getPersistentIdentifier();
		try (AutoClosingIterable<Number> i = p.newSQL("select count(1) from " + table).scalarIterable(Number.class)) {
			boolean found = false;
			for (Number n : i) {
				assertNotNull(n);
				found = true;
			}
			assertTrue(found);
		}
	}

	@Test
	void testSQLTupleResults() throws Exception {
		AllAttributesPersistent saved = Util.constructRandomInstance(u, m, aapd, 1);
		saved = p.save(saved);
		String table = aapd.getPersistent().getPersistentIdentifier();
		List<Object[]> tuples = p.newSQL("select bizId, bizVersion from " + table + " where bizId = :id")
				.putParameter("id", saved.getBizId(), false)
				.tupleResults();
		assertEquals(1, tuples.size());
		assertEquals(2, tuples.get(0).length);
	}

	@Test
	void testSQLTupleIterable() throws Exception {
		AllAttributesPersistent saved = Util.constructRandomInstance(u, m, aapd, 1);
		p.save(saved);
		String table = aapd.getPersistent().getPersistentIdentifier();
		int count = 0;
		try (AutoClosingIterable<Object[]> i = p.newSQL("select bizId, bizVersion from " + table).tupleIterable()) {
			for (Object[] row : i) {
				assertEquals(2, row.length);
				count++;
			}
		}
		assertTrue(count >= 1);
	}

	@Test
	void testUpsertCollectionTuples() throws Exception {
		AllAttributesPersistent test = Util.constructRandomInstance(u, m, aapd, 2);
		test = p.save(test);
		assertFalse(test.getAggregatedCollection().isEmpty());

		// Remove the junction table entries manually
		String table = aapd.getPersistent().getPersistentIdentifier();
		p.newSQL("delete from " + table + "_aggregatedCollection where owner_id = :id")
				.putParameter("id", test.getBizId(), false)
				.execute();

		// upsert should re-insert them
		((AbstractHibernatePersistence) p).upsertCollectionTuples(test, AllAttributesPersistent.aggregatedCollectionPropertyName);

		int junctionCount = p.newSQL("select count(1) from " + table + "_aggregatedCollection where owner_id = :id")
				.putParameter("id", test.getBizId(), false)
				.scalarResult(Number.class).intValue();
		assertEquals(test.getAggregatedCollection().size(), junctionCount);
	}

	@Test
	void testInsertCollectionTuples() throws Exception {
		AllAttributesPersistent test = Util.constructRandomInstance(u, m, aapd, 2);
		test = p.save(test);
		assertFalse(test.getAggregatedCollection().isEmpty());

		// Remove the junction table entries manually
		String table = aapd.getPersistent().getPersistentIdentifier();
		p.newSQL("delete from " + table + "_aggregatedCollection where owner_id = :id")
				.putParameter("id", test.getBizId(), false)
				.execute();

		// insert should add them back
		((AbstractHibernatePersistence) p).insertCollectionTuples(test, AllAttributesPersistent.aggregatedCollectionPropertyName);

		int junctionCount = p.newSQL("select count(1) from " + table + "_aggregatedCollection where owner_id = :id")
				.putParameter("id", test.getBizId(), false)
				.scalarResult(Number.class).intValue();
		assertEquals(test.getAggregatedCollection().size(), junctionCount);
	}

	@Test
	void testWithDocumentPermissionScopesFunction() throws Exception {
		AllAttributesPersistent saved = Util.constructRandomInstance(u, m, aapd, 1);
		saved = p.save(saved);
		final String bizId = saved.getBizId();
		String retrieved = ((AbstractHibernatePersistence) p).withDocumentPermissionScopes(
				DocumentPermissionScope.customer,
				persistence -> {
					try {
						AllAttributesPersistent result = persistence.retrieve(
								AllAttributesPersistent.MODULE_NAME,
								AllAttributesPersistent.DOCUMENT_NAME,
								bizId);
						return result != null ? result.getBizId() : null;
					}
					catch (@SuppressWarnings("unused") Exception ignored) {
						return null;
					}
				});
		assertEquals(bizId, retrieved);
	}

	@Test
	void testWithDocumentPermissionScopesConsumer() throws Exception {
		// Consumer overload — just verify it executes without error
		((AbstractHibernatePersistence) p).withDocumentPermissionScopes(
				DocumentPermissionScope.global,
				persistence -> {
					// no-op — just exercises the set/reset path
				});
	}

	@Test
	void testRetrieveByDocument() throws Exception {
		AllAttributesPersistent saved = Util.constructRandomInstance(u, m, aapd, 1);
		saved = p.save(saved);
		AllAttributesPersistent retrieved = ((AbstractHibernatePersistence) p).retrieve(aapd, saved.getBizId());
		assertNotNull(retrieved);
		assertEquals(saved.getBizId(), retrieved.getBizId());
	}

	@Test
	void testRetrieveNonExistentByDocument() throws Exception {
		AllAttributesPersistent retrieved = ((AbstractHibernatePersistence) p).retrieve(aapd, "non-existent-id");
		assertNull(retrieved);
	}

	@Test
	void testRetrieveAndLockByDocument() throws Exception {
		AllAttributesPersistent saved = Util.constructRandomInstance(u, m, aapd, 1);
		saved = p.save(saved);
		AllAttributesPersistent retrieved = ((AbstractHibernatePersistence) p).retrieveAndLock(aapd, saved.getBizId());
		assertNotNull(retrieved);
		assertEquals(saved.getBizId(), retrieved.getBizId());
	}

	@Test
	void testRetrieveAndLockNonExistentThrows() throws Exception {
		assertThrows(NoResultsException.class, () -> {
			((AbstractHibernatePersistence) p).retrieveAndLock(aapd, "non-existent-id");
		});
	}

	@Test
	void testSharedCacheBeanByReference() throws Exception {
		AllAttributesPersistent saved = Util.constructRandomInstance(u, m, aapd, 1);
		saved = p.save(saved);
		// sharedCacheBean returns true when the bean is in the second-level cache.
		// H2 in-memory has no second-level cache configured, so false is expected.
		boolean cached = ((AbstractHibernatePersistence) p).sharedCacheBean(saved);
		assertFalse(cached);
	}

	@Test
	void testSharedCacheBeanByIds() throws Exception {
		AllAttributesPersistent saved = Util.constructRandomInstance(u, m, aapd, 1);
		saved = p.save(saved);
		boolean cached = ((AbstractHibernatePersistence) p).sharedCacheBean(
				AllAttributesPersistent.MODULE_NAME,
				AllAttributesPersistent.DOCUMENT_NAME,
				saved.getBizId());
		assertFalse(cached);
	}

	@Test
	void testSharedCacheCollectionByReference() throws Exception {
		AllAttributesPersistent saved = Util.constructRandomInstance(u, m, aapd, 2);
		saved = p.save(saved);
		boolean cached = ((AbstractHibernatePersistence) p).sharedCacheCollection(
				saved,
				AllAttributesPersistent.aggregatedCollectionPropertyName);
		assertFalse(cached);
	}

	@Test
	void testSharedCacheCollectionByIds() throws Exception {
		AllAttributesPersistent saved = Util.constructRandomInstance(u, m, aapd, 2);
		saved = p.save(saved);
		boolean cached = ((AbstractHibernatePersistence) p).sharedCacheCollection(
				AllAttributesPersistent.MODULE_NAME,
				AllAttributesPersistent.DOCUMENT_NAME,
				AllAttributesPersistent.aggregatedCollectionPropertyName,
				saved.getBizId());
		assertFalse(cached);
	}

	@Test
	void testEvictSharedCacheBeansNoArg() throws Exception {
		AllAttributesPersistent saved = Util.constructRandomInstance(u, m, aapd, 1);
		p.save(saved);
		((AbstractHibernatePersistence) p).evictSharedCacheBeans();
	}

	@Test
	void testEvictSharedCacheBeansByModuleDocument() throws Exception {
		AllAttributesPersistent saved = Util.constructRandomInstance(u, m, aapd, 1);
		p.save(saved);
		((AbstractHibernatePersistence) p).evictSharedCacheBeans(
				AllAttributesPersistent.MODULE_NAME,
				AllAttributesPersistent.DOCUMENT_NAME);
	}

	@Test
	void testEvictSharedCachedBeanByReference() throws Exception {
		AllAttributesPersistent saved = Util.constructRandomInstance(u, m, aapd, 1);
		saved = p.save(saved);
		((AbstractHibernatePersistence) p).evictSharedCachedBean(saved);
	}

	@Test
	void testEvictSharedCachedBeanByIds() throws Exception {
		AllAttributesPersistent saved = Util.constructRandomInstance(u, m, aapd, 1);
		saved = p.save(saved);
		((AbstractHibernatePersistence) p).evictSharedCachedBean(
				AllAttributesPersistent.MODULE_NAME,
				AllAttributesPersistent.DOCUMENT_NAME,
				saved.getBizId());
	}

	@Test
	void testEvictSharedCacheCollectionsNoArg() throws Exception {
		AllAttributesPersistent saved = Util.constructRandomInstance(u, m, aapd, 2);
		p.save(saved);
		((AbstractHibernatePersistence) p).evictSharedCacheCollections();
	}

	@Test
	void testEvictSharedCacheCollectionsByModuleDocumentCollection() throws Exception {
		AllAttributesPersistent saved = Util.constructRandomInstance(u, m, aapd, 2);
		p.save(saved);
		((AbstractHibernatePersistence) p).evictSharedCacheCollections(
				AllAttributesPersistent.MODULE_NAME,
				AllAttributesPersistent.DOCUMENT_NAME,
				AllAttributesPersistent.aggregatedCollectionPropertyName);
	}

	@Test
	void testEvictSharedCacheCollectionByReference() throws Exception {
		AllAttributesPersistent saved = Util.constructRandomInstance(u, m, aapd, 2);
		saved = p.save(saved);
		((AbstractHibernatePersistence) p).evictSharedCacheCollection(
				saved,
				AllAttributesPersistent.aggregatedCollectionPropertyName);
	}

	@Test
	void testEvictSharedCacheCollectionByIds() throws Exception {
		AllAttributesPersistent saved = Util.constructRandomInstance(u, m, aapd, 2);
		saved = p.save(saved);
		((AbstractHibernatePersistence) p).evictSharedCacheCollection(
				AllAttributesPersistent.MODULE_NAME,
				AllAttributesPersistent.DOCUMENT_NAME,
				AllAttributesPersistent.aggregatedCollectionPropertyName,
				saved.getBizId());
	}

	@Test
	void testBizQLWithDocumentResolution() throws Exception {
		AllAttributesPersistent saved = Util.constructRandomInstance(u, m, aapd, 1);
		p.save(saved);
		List<AllAttributesPersistent> results = p.newBizQL(
				"select bean from {test.AllAttributesPersistent} as bean")
				.beanResults();
		assertFalse(results.isEmpty());
	}

	@Test
	void testBizQLScalarResultsWithDocumentResolution() throws Exception {
		AllAttributesPersistent saved = Util.constructRandomInstance(u, m, aapd, 1);
		p.save(saved);
		List<String> results = p.newBizQL(
				"select bean.bizId as bizId from {test.AllAttributesPersistent} as bean")
				.scalarResults(String.class);
		assertFalse(results.isEmpty());
	}

	@Test
	void testDocumentQueryBeanResultsWithData() throws Exception {
		AllAttributesPersistent saved = Util.constructRandomInstance(u, m, aapd, 1);
		p.save(saved);
		List<AllAttributesPersistent> results = p.newDocumentQuery(
				AllAttributesPersistent.MODULE_NAME,
				AllAttributesPersistent.DOCUMENT_NAME)
				.beanResults();
		assertFalse(results.isEmpty());
	}

	@Test
	void testGenerateDDLCreateScript() throws Exception {
		List<String> createDDL = new ArrayList<>();
		((AbstractHibernatePersistence) p).generateDDL(null, createDDL, null);
			assertTrue(! createDDL.isEmpty(), "DDL create script should have been generated");
	}

	@Test
	void testGenerateDDLDropScript() throws Exception {
		List<String> dropDDL = new ArrayList<>();
		((AbstractHibernatePersistence) p).generateDDL(dropDDL, null, null);
			assertTrue(! dropDDL.isEmpty(), "DDL drop script should have been generated");
	}

	@Test
	void testDocumentQueryBeanIterableWithData() throws Exception {
		AllAttributesPersistent saved = Util.constructRandomInstance(u, m, aapd, 1);
		p.save(saved);
		int count = 0;
		try (AutoClosingIterable<AllAttributesPersistent> i = p.newDocumentQuery(
				AllAttributesPersistent.MODULE_NAME,
				AllAttributesPersistent.DOCUMENT_NAME)
				.beanIterable()) {
			for (@SuppressWarnings("unused") AllAttributesPersistent bean : i) {
				count++;
			}
		}
		assertTrue(count >= 1);
	}

	@Test
	void testDocumentQueryProjectedResultsWithData() throws Exception {
		AllAttributesPersistent saved = Util.constructRandomInstance(u, m, aapd, 1);
		p.save(saved);
		List<?> results = p.newDocumentQuery(
				AllAttributesPersistent.MODULE_NAME,
				AllAttributesPersistent.DOCUMENT_NAME)
				.projectedResults();
		assertFalse(results.isEmpty());
	}

	@Test
	void testDocumentQueryProjectedIterableWithData() throws Exception {
		AllAttributesPersistent saved = Util.constructRandomInstance(u, m, aapd, 1);
		p.save(saved);
		int count = 0;
		try (AutoClosingIterable<?> i = p.newDocumentQuery(
				AllAttributesPersistent.MODULE_NAME,
				AllAttributesPersistent.DOCUMENT_NAME)
				.projectedIterable()) {
			for (@SuppressWarnings("unused") Object bean : i) {
				count++;
			}
		}
		assertTrue(count >= 1);
	}

	@Test
	void testDocumentQueryScalarResultsWithData() throws Exception {
		AllAttributesPersistent saved = Util.constructRandomInstance(u, m, aapd, 1);
		p.save(saved);
		DocumentQuery dq = p.newDocumentQuery(
				AllAttributesPersistent.MODULE_NAME,
				AllAttributesPersistent.DOCUMENT_NAME);
		dq.addBoundProjection(DocumentQuery.THIS_ALIAS, Bean.BIZ_KEY, Bean.BIZ_KEY);
		List<String> results = dq.scalarResults(String.class);
		assertFalse(results.isEmpty());
	}

	@Test
	void testDocumentQueryScalarIterableWithData() throws Exception {
		AllAttributesPersistent saved = Util.constructRandomInstance(u, m, aapd, 1);
		p.save(saved);
		DocumentQuery dq = p.newDocumentQuery(
				AllAttributesPersistent.MODULE_NAME,
				AllAttributesPersistent.DOCUMENT_NAME);
		dq.addBoundProjection(DocumentQuery.THIS_ALIAS, Bean.BIZ_KEY, Bean.BIZ_KEY);
		try (AutoClosingIterable<String> i = dq.scalarIterable(String.class)) {
			boolean found = false;
			for (String val : i) {
				assertNotNull(val);
				found = true;
			}
			assertTrue(found);
		}
	}

	@Test
	void testDocumentQueryTupleResultsWithData() throws Exception {
		AllAttributesPersistent saved = Util.constructRandomInstance(u, m, aapd, 1);
		p.save(saved);
		DocumentQuery dq = p.newDocumentQuery(
				AllAttributesPersistent.MODULE_NAME,
				AllAttributesPersistent.DOCUMENT_NAME);
		dq.addBoundProjection(DocumentQuery.THIS_ALIAS, Bean.DOCUMENT_ID, "a");
		dq.addBoundProjection(DocumentQuery.THIS_ALIAS, Bean.BIZ_KEY, "b");
		List<Object[]> results = dq.tupleResults();
		assertFalse(results.isEmpty());
		assertEquals(2, results.get(0).length);
	}

	@Test
	void testDocumentQueryTupleIterableWithData() throws Exception {
		AllAttributesPersistent saved = Util.constructRandomInstance(u, m, aapd, 1);
		p.save(saved);
		DocumentQuery dq = p.newDocumentQuery(
				AllAttributesPersistent.MODULE_NAME,
				AllAttributesPersistent.DOCUMENT_NAME);
		dq.addBoundProjection(DocumentQuery.THIS_ALIAS, Bean.DOCUMENT_ID, "a");
		dq.addBoundProjection(DocumentQuery.THIS_ALIAS, Bean.BIZ_KEY, "b");
		int count = 0;
		try (AutoClosingIterable<Object[]> i = dq.tupleIterable()) {
			for (Object[] row : i) {
				assertEquals(2, row.length);
				count++;
			}
		}
		assertTrue(count >= 1);
	}

	@Test
	void testBizQLBeanIterableWithDocumentResolution() throws Exception {
		AllAttributesPersistent saved = Util.constructRandomInstance(u, m, aapd, 1);
		p.save(saved);
		int count = 0;
		try (AutoClosingIterable<AllAttributesPersistent> i = p.newBizQL(
				"select bean from {test.AllAttributesPersistent} as bean")
				.beanIterable()) {
			for (@SuppressWarnings("unused") AllAttributesPersistent bean : i) {
				count++;
			}
		}
		assertTrue(count >= 1);
	}

	@Test
	void testBizQLProjectedResultsWithDocumentResolution() throws Exception {
		AllAttributesPersistent saved = Util.constructRandomInstance(u, m, aapd, 1);
		p.save(saved);
		// projectedResults() maps aliases to DynamicBean properties; explicit column alias is required
		List<?> results = p.newBizQL(
				"select bean.bizKey as k from {test.AllAttributesPersistent} as bean")
				.projectedResults();
		assertFalse(results.isEmpty());
	}

	@Test
	void testBizQLTupleResultsWithDocumentResolution() throws Exception {
		AllAttributesPersistent saved = Util.constructRandomInstance(u, m, aapd, 1);
		p.save(saved);
		List<Object[]> results = p.newBizQL(
				"select bean.bizId as a, bean.bizKey as b from {test.AllAttributesPersistent} as bean")
				.tupleResults();
		assertFalse(results.isEmpty());
		assertEquals(2, results.get(0).length);
	}

	@Test
	void testBizQLExecuteDeleteWithDocumentResolution() throws Exception {
		int deleted = p.newBizQL(
				"delete from {test.AllAttributesPersistent} where bizKey = 'this-key-does-not-exist-xyz'")
				.execute();
		assertTrue(deleted >= 0);
	}

	@Test
	void testMergeListOfBeans() throws Exception {
		AllAttributesPersistent test1 = Util.constructRandomInstance(u, m, aapd, 1);
		AllAttributesPersistent test2 = Util.constructRandomInstance(u, m, aapd, 1);
		List<AllAttributesPersistent> merged = p.merge(List.of(test1, test2));
		assertEquals(2, merged.size());
		assertNotNull(merged.get(0).getBizId());
		assertNotNull(merged.get(1).getBizId());
	}

	@Test
	void testMergeVarargsOfBeans() throws Exception {
		AllAttributesPersistent test1 = Util.constructRandomInstance(u, m, aapd, 1);
		AllAttributesPersistent test2 = Util.constructRandomInstance(u, m, aapd, 1);
		List<AllAttributesPersistent> merged = p.merge(test1, test2);
		assertEquals(2, merged.size());
		assertNotNull(merged.get(0).getBizId());
		assertNotNull(merged.get(1).getBizId());
	}

	@Test
	void testEvictCachedBean() throws Exception {
		AllAttributesPersistent saved = Util.constructRandomInstance(u, m, aapd, 1);
		saved = p.save(saved);
		// Evicting a managed instance should not throw
		p.evictCached(saved);
	}

	@Test
	void testDocumentQueryByExampleBean() throws Exception {
		AllAttributesPersistent saved = Util.constructRandomInstance(u, m, aapd, 1);
		saved = p.save(saved);
		// Use the saved bean as a query-by-example: newDocumentQuery(Bean) overload
		List<AllAttributesPersistent> results = p.newDocumentQuery(saved).beanResults();
		assertFalse(results.isEmpty());
	}

	@Test
	void testBizQLWithInListParameter() throws Exception {
		AllAttributesPersistent saved1 = Util.constructRandomInstance(u, m, aapd, 1);
		saved1 = p.save(saved1);
		AllAttributesPersistent saved2 = Util.constructRandomInstance(u, m, aapd, 1);
		saved2 = p.save(saved2);

		List<String> ids = List.of(saved1.getBizId(), saved2.getBizId());
		List<AllAttributesPersistent> results = p.newBizQL(
				"select bean from {test.AllAttributesPersistent} as bean where bean.bizId in (:ids)")
				.putParameter("ids", ids)
				.beanResults();
		assertEquals(2, results.size());
	}

	@Test
	void testDocumentQueryWithFromAndFilterClauses() throws Exception {
		AllAttributesPersistent saved = Util.constructRandomInstance(u, m, aapd, 1);
		saved = p.save(saved);
		final String bizId = saved.getBizId();
		// newDocumentQuery(Document, fromClause, filterClause, groupClause, orderClause) overload
		DocumentQuery dq = p.newDocumentQuery(aapd,
				null, // fromClause (null = default)
				"bean.bizId = :id",
				null,
				null);
		dq.putParameter("id", bizId);
		List<AllAttributesPersistent> results = dq.beanResults();
		assertEquals(1, results.size());
		assertEquals(bizId, results.get(0).getBizId());
	}
}
