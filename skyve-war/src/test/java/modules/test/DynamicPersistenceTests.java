package modules.test;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import java.util.List;

import org.junit.jupiter.api.Test;
import org.locationtech.jts.geom.Geometry;
import org.skyve.domain.Bean;
import org.skyve.domain.ChildBean;
import org.skyve.domain.DynamicPersistentBean;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.messages.ReferentialConstraintViolationException;
import org.skyve.domain.types.DateOnly;
import org.skyve.domain.types.DateTime;
import org.skyve.domain.types.Decimal10;
import org.skyve.domain.types.Decimal2;
import org.skyve.domain.types.Decimal5;
import org.skyve.domain.types.TimeOnly;
import org.skyve.domain.types.Timestamp;
import org.skyve.util.Util;

import modules.test.domain.AllAttributesPersistent;
import modules.test.domain.AllDynamicAttributesPersistent;

@SuppressWarnings("null")
class DynamicPersistenceTests extends AbstractSkyveTestDispose {

	@Test
	void testHasDynamic() {
		assertFalse(aapd.hasDynamic());
		assertTrue(adapd.hasDynamic()); // has dynamic attributes
		assertTrue(aadpd.hasDynamic()); // is dynamic
		assertFalse(aarpd.hasDynamic());
		assertFalse(ad1.hasDynamic());
		assertFalse(ad2.hasDynamic());
		assertFalse(ao2m.hasDynamic());
		assertFalse(ao2o.hasDynamic());
		assertTrue(dmed.hasDynamic()); // is dynamic
		assertTrue(dmsd.hasDynamic()); // is dynamic
		assertFalse(hd.hasDynamic());
		assertFalse(im2mpd.hasDynamic());
		assertFalse(io2mpd.hasDynamic());
		assertFalse(io2opd.hasDynamic());
		assertTrue(mbd.hasDynamic()); // points to messd (dynamic)
		assertTrue(mejsd.hasDynamic()); // has dynamic attribute
		assertTrue(messd.hasDynamic()); // has dynamic attribute
		assertTrue(msjsd.hasDynamic()); // has dynamic attribute
		assertTrue(msssd.hasDynamic()); // has dynamic attribute
		assertFalse(rd.hasDynamic());
		assertFalse(ucn.hasDynamic());
		assertFalse(ucnn.hasDynamic());
	}

	@Test
	@SuppressWarnings("unchecked")
	void testPersistenceOfDynamicAttributes() throws Exception {
		AllDynamicAttributesPersistent test = Util.constructRandomInstance(u, m, adapd, 2);
		test = p.save(test);
		test = p.save(test);
		test = p.save(test);
		test = p.save(test);
		test = p.save(test);
		test = p.save(test);
		test = p.save(test);

		assertEquals(Integer.valueOf(1), test.getBizVersion());
		assertEquals(Integer.valueOf(1), test.getAggregatedAssociation().getBizVersion());
		assertEquals(Integer.valueOf(0), test.getComposedAssociation().getBizVersion());
		assertEquals(null, test.getEmbeddedAssociation().getBizVersion());
		assertEquals(Integer.valueOf(0), ((DynamicPersistentBean) test.getDynamic(AllDynamicAttributesPersistent.dynamicAggregatedAssociationPropertyName)).getBizVersion());
		assertEquals(Integer.valueOf(0), ((DynamicPersistentBean) test.getDynamic(AllDynamicAttributesPersistent.dynamicComposedAssociationPropertyName)).getBizVersion());
		assertEquals(Integer.valueOf(0), ((DynamicPersistentBean) test.getDynamic(AllDynamicAttributesPersistent.dynamicEmbeddedAssociationPropertyName)).getBizVersion());
		assertEquals(Integer.valueOf(0), test.getComposedCollection().get(0).getBizVersion());
		assertEquals(Integer.valueOf(0), test.getComposedCollection().get(1).getBizVersion());
		List<DynamicPersistentBean> list = (List<DynamicPersistentBean>) test.getDynamic(AllDynamicAttributesPersistent.dynamicAggregatedCollectionPropertyName);
		assertEquals(Integer.valueOf(0), list.get(0).getBizVersion());
		assertEquals(Integer.valueOf(0), list.get(1).getBizVersion());
		list = (List<DynamicPersistentBean>) test.getDynamic(AllDynamicAttributesPersistent.dynamicComposedCollectionPropertyName);
		assertEquals(Integer.valueOf(0), list.get(0).getBizVersion());
		assertEquals(Integer.valueOf(0), list.get(1).getBizVersion());
		list = (List<DynamicPersistentBean>) test.getDynamic(AllDynamicAttributesPersistent.dynamicChildCollectionPropertyName);
		assertEquals(Integer.valueOf(0), list.get(0).getBizVersion());
		assertEquals(Integer.valueOf(0), list.get(1).getBizVersion());
	}

	@Test
	void testCoercionOfDynamicAttributes() throws Exception {
		AllDynamicAttributesPersistent test = Util.constructRandomInstance(u, m, adapd, 1);
		test.setDynamic(AllDynamicAttributesPersistent.enum3PropertyName, "one");
		test = p.save(test);
		
		p.evictAllCached();
		test = p.retrieve(adapd, test.getBizId());
		assertNotNull(test);

		assertTrue(test.getDynamic(AllDynamicAttributesPersistent.booleanFlagPropertyName) instanceof Boolean);
		assertTrue(test.getDynamic(AllDynamicAttributesPersistent.datePropertyName) instanceof DateOnly);
		assertTrue(test.getDynamic(AllDynamicAttributesPersistent.dateTimePropertyName) instanceof DateTime);
		assertTrue(test.getDynamic(AllDynamicAttributesPersistent.decimal10PropertyName) instanceof Decimal10);
		assertTrue(test.getDynamic(AllDynamicAttributesPersistent.decimal2PropertyName) instanceof Decimal2);
		assertTrue(test.getDynamic(AllDynamicAttributesPersistent.decimal5PropertyName) instanceof Decimal5);
		assertTrue(test.getDynamic(AllDynamicAttributesPersistent.enum3PropertyName) instanceof String);
		assertTrue(test.getDynamic(AllDynamicAttributesPersistent.geometryPropertyName) instanceof Geometry);
		assertTrue(test.getDynamic(AllDynamicAttributesPersistent.longIntegerPropertyName) instanceof Long);
		assertTrue(test.getDynamic(AllDynamicAttributesPersistent.normalIntegerPropertyName) instanceof Integer);
		assertTrue(test.getDynamic(AllDynamicAttributesPersistent.timePropertyName) instanceof TimeOnly);
		assertTrue(test.getDynamic(AllDynamicAttributesPersistent.timestampPropertyName) instanceof Timestamp);
	}
	
	@Test
	@SuppressWarnings("unchecked")
	void testRetrievalOfDynamicAttributes() throws Exception {
		AllDynamicAttributesPersistent test = Util.constructRandomInstance(u, m, adapd, 2);

		test.setDynamic(AllDynamicAttributesPersistent.colourPropertyName, "#000001");
		test.getAggregatedAssociation().setDynamic(AllDynamicAttributesPersistent.colourPropertyName, "#000002");
		test.getComposedAssociation().setDynamic(AllDynamicAttributesPersistent.colourPropertyName, "#000003");
		test.getEmbeddedAssociation().setColour("#000004");

		PersistentBean bean = (PersistentBean) test.getDynamic(AllDynamicAttributesPersistent.dynamicAggregatedAssociationPropertyName);
		bean.setDynamic(AllAttributesPersistent.colourPropertyName, "#000005");
		
		bean = (PersistentBean) test.getDynamic(AllDynamicAttributesPersistent.dynamicComposedAssociationPropertyName);
		bean.setDynamic(AllAttributesPersistent.colourPropertyName, "#000006");
		
		bean = (PersistentBean) test.getDynamic(AllDynamicAttributesPersistent.dynamicEmbeddedAssociationPropertyName);
		bean.setDynamic(AllAttributesPersistent.colourPropertyName, "#000007");

		test.getComposedCollection().get(0).setDynamic(AllDynamicAttributesPersistent.colourPropertyName, "#000008");
		test.getComposedCollection().get(1).setDynamic(AllDynamicAttributesPersistent.colourPropertyName, "#000009");
		
		List<PersistentBean> list = (List<PersistentBean>) test.getDynamic(AllDynamicAttributesPersistent.dynamicAggregatedCollectionPropertyName);
		list.get(0).setDynamic(AllDynamicAttributesPersistent.colourPropertyName, "#000010");
		list.get(1).setDynamic(AllDynamicAttributesPersistent.colourPropertyName, "#000011");
		
		list = (List<PersistentBean>) test.getDynamic(AllDynamicAttributesPersistent.dynamicComposedCollectionPropertyName);
		list.get(0).setDynamic(AllDynamicAttributesPersistent.colourPropertyName, "#000012");
		list.get(1).setDynamic(AllDynamicAttributesPersistent.colourPropertyName, "#000013");

		list = (List<PersistentBean>) test.getDynamic(AllDynamicAttributesPersistent.dynamicChildCollectionPropertyName);
		list.get(0).setDynamic(AllDynamicAttributesPersistent.colourPropertyName, "#000014");
		list.get(1).setDynamic(AllDynamicAttributesPersistent.colourPropertyName, "#000015");

		test = p.save(test);
		p.evictAllCached();
		
		AllDynamicAttributesPersistent clone = p.retrieve(adapd, test.getBizId());
		assertNotNull(clone);
		
		assertEquals("#000001", clone.getDynamic(AllDynamicAttributesPersistent.colourPropertyName));
		assertEquals("#000002", clone.getAggregatedAssociation().getDynamic(AllDynamicAttributesPersistent.colourPropertyName));
		assertEquals("#000003", clone.getComposedAssociation().getDynamic(AllDynamicAttributesPersistent.colourPropertyName));
		assertEquals("#000004", clone.getEmbeddedAssociation().getColour());

		assertEquals("#000005", ((Bean) clone.getDynamic(AllDynamicAttributesPersistent.dynamicAggregatedAssociationPropertyName)).getDynamic(AllDynamicAttributesPersistent.colourPropertyName));
		assertEquals("#000006", ((Bean) clone.getDynamic(AllDynamicAttributesPersistent.dynamicComposedAssociationPropertyName)).getDynamic(AllDynamicAttributesPersistent.colourPropertyName));
		assertEquals("#000007", ((Bean) clone.getDynamic(AllDynamicAttributesPersistent.dynamicEmbeddedAssociationPropertyName)).getDynamic(AllDynamicAttributesPersistent.colourPropertyName));
		
		assertEquals("#000008", clone.getComposedCollection().get(0).getDynamic(AllDynamicAttributesPersistent.colourPropertyName));
		assertEquals("#000009", clone.getComposedCollection().get(1).getDynamic(AllDynamicAttributesPersistent.colourPropertyName));
		
		list = (List<PersistentBean>) clone.getDynamic(AllDynamicAttributesPersistent.dynamicAggregatedCollectionPropertyName);
		assertEquals("#000010", list.get(0).getDynamic(AllDynamicAttributesPersistent.colourPropertyName));
		assertEquals("#000011", list.get(1).getDynamic(AllDynamicAttributesPersistent.colourPropertyName));
		
		list = (List<PersistentBean>) clone.getDynamic(AllDynamicAttributesPersistent.dynamicComposedCollectionPropertyName);
		assertEquals("#000012", list.get(0).getDynamic(AllDynamicAttributesPersistent.colourPropertyName));
		assertEquals("#000013", list.get(1).getDynamic(AllDynamicAttributesPersistent.colourPropertyName));

		List<ChildBean<Bean>> children = (List<ChildBean<Bean>>) clone.getDynamic(AllDynamicAttributesPersistent.dynamicChildCollectionPropertyName);
		assertEquals("#000014", children.get(0).getDynamic(AllDynamicAttributesPersistent.colourPropertyName));
		assertEquals("#000015", children.get(1).getDynamic(AllDynamicAttributesPersistent.colourPropertyName));
		assertSame(clone, children.get(0).getParent());
		assertSame(clone, children.get(1).getParent());
	}

	@Test
	void testDeletionOfDynamicAttributes() throws Exception {
		AllDynamicAttributesPersistent test = Util.constructRandomInstance(u, m, adapd, 3); // to get static -> dynamic -> static bean graph
		test = p.save(test);

		// Test
		assertEquals(2, p.newSQL("select count(1) from TEST_AllAttributesPersistent").scalarResult(Number.class).intValue());
		assertEquals(59, p.newSQL("select count(1) from TEST_AllDynamicAttributesPersistent").scalarResult(Number.class).intValue());
		assertEquals(160, p.newSQL("select count(1) from ADM_DynamicEntity").scalarResult(Number.class).intValue());
		assertEquals(141, p.newSQL("select count(1) from ADM_DynamicRelation").scalarResult(Number.class).intValue());
		
		p.delete(test);
		
		// Test
		assertEquals(2, p.newSQL("select count(1) from TEST_AllAttributesPersistent").scalarResult(Number.class).intValue());
		assertEquals(36, p.newSQL("select count(1) from TEST_AllDynamicAttributesPersistent").scalarResult(Number.class).intValue());
		assertEquals(94, p.newSQL("select count(1) from ADM_DynamicEntity").scalarResult(Number.class).intValue());
		assertEquals(54, p.newSQL("select count(1) from ADM_DynamicRelation").scalarResult(Number.class).intValue());
	}
	
	@Test
	@SuppressWarnings("java:S1854") // Persistence.save() may return a different instance; assignment captured intentionally
	void testConstraintViolationOnDeletionOfDynamicAttributes() throws Exception {
		PersistentBean referenced = Util.constructRandomInstance(u, m, adapd, 1);
		referenced = p.save(referenced);

		PersistentBean referrer = Util.constructRandomInstance(u, m, aadpd, 1);
		referrer.setDynamic(AllDynamicAttributesPersistent.dynamicComposedAssociationPropertyName, referenced);
		referrer = p.save(referrer);

		final PersistentBean toDelete = referenced;
		ReferentialConstraintViolationException rcve = assertThrows(ReferentialConstraintViolationException.class, () -> p.delete(toDelete));

		assertThat(rcve.getMessage(), is(notNullValue()));
	}

	@Test
	@SuppressWarnings("unchecked")
	void testPersistenceOfDynamicDocument() throws Exception {
		PersistentBean test = Util.constructRandomInstance(u, m, aadpd, 2);
		test = p.save(test);
		test = p.save(test);

		assertEquals(Integer.valueOf(0), test.getBizVersion());
		assertEquals(Integer.valueOf(0), ((PersistentBean) test.getDynamic(AllDynamicAttributesPersistent.aggregatedAssociationPropertyName)).getBizVersion());
		assertEquals(Integer.valueOf(0), ((PersistentBean) test.getDynamic(AllDynamicAttributesPersistent.composedAssociationPropertyName)).getBizVersion());
		assertEquals(Integer.valueOf(0), ((DynamicPersistentBean) test.getDynamic(AllDynamicAttributesPersistent.dynamicAggregatedAssociationPropertyName)).getBizVersion());
		assertEquals(Integer.valueOf(0), ((DynamicPersistentBean) test.getDynamic(AllDynamicAttributesPersistent.dynamicComposedAssociationPropertyName)).getBizVersion());
		assertEquals(Integer.valueOf(0), ((DynamicPersistentBean) test.getDynamic(AllDynamicAttributesPersistent.dynamicEmbeddedAssociationPropertyName)).getBizVersion());

		List<PersistentBean> list = (List<PersistentBean>) test.getDynamic(AllDynamicAttributesPersistent.composedCollectionPropertyName);
		assertEquals(Integer.valueOf(0), list.get(0).getBizVersion());
		assertEquals(Integer.valueOf(0), list.get(1).getBizVersion());
		list = (List<PersistentBean>) test.getDynamic(AllDynamicAttributesPersistent.dynamicAggregatedCollectionPropertyName);
		assertEquals(Integer.valueOf(0), list.get(0).getBizVersion());
		assertEquals(Integer.valueOf(0), list.get(1).getBizVersion());
		list = (List<PersistentBean>) test.getDynamic(AllDynamicAttributesPersistent.dynamicComposedCollectionPropertyName);
		assertEquals(Integer.valueOf(0), list.get(0).getBizVersion());
		assertEquals(Integer.valueOf(0), list.get(1).getBizVersion());
		list = (List<PersistentBean>) test.getDynamic(AllDynamicAttributesPersistent.dynamicChildCollectionPropertyName);
		assertEquals(Integer.valueOf(0), list.get(0).getBizVersion());
		assertEquals(Integer.valueOf(0), list.get(1).getBizVersion());
	}

	@Test
	@SuppressWarnings("unchecked")
	void testRetrievalOfDynamicDocument() throws Exception {
		PersistentBean test = Util.constructRandomInstance(u, m, aadpd, 2);
		test.setDynamic(AllDynamicAttributesPersistent.colourPropertyName, "#000001");
		((Bean) test.getDynamic(AllDynamicAttributesPersistent.aggregatedAssociationPropertyName)).setDynamic(AllDynamicAttributesPersistent.colourPropertyName, "#000002");
		((Bean) test.getDynamic(AllDynamicAttributesPersistent.composedAssociationPropertyName)).setDynamic(AllDynamicAttributesPersistent.colourPropertyName, "#000003");

		PersistentBean bean = (PersistentBean) test.getDynamic(AllDynamicAttributesPersistent.dynamicAggregatedAssociationPropertyName);
		bean.setDynamic(AllAttributesPersistent.colourPropertyName, "#000005");
		
		bean = (PersistentBean) test.getDynamic(AllDynamicAttributesPersistent.dynamicComposedAssociationPropertyName);
		bean.setDynamic(AllAttributesPersistent.colourPropertyName, "#000006");
		
		bean = (PersistentBean) test.getDynamic(AllDynamicAttributesPersistent.dynamicEmbeddedAssociationPropertyName);
		bean.setDynamic(AllAttributesPersistent.colourPropertyName, "#000007");
		
		List<PersistentBean> list = (List<PersistentBean>) test.getDynamic(AllDynamicAttributesPersistent.composedCollectionPropertyName);
		list.get(0).setDynamic(AllDynamicAttributesPersistent.colourPropertyName, "#000010");
		list.get(1).setDynamic(AllDynamicAttributesPersistent.colourPropertyName, "#000011");
		
		list = (List<PersistentBean>) test.getDynamic(AllDynamicAttributesPersistent.dynamicAggregatedCollectionPropertyName);
		list.get(0).setDynamic(AllDynamicAttributesPersistent.colourPropertyName, "#000010");
		list.get(1).setDynamic(AllDynamicAttributesPersistent.colourPropertyName, "#000011");
		
		list = (List<PersistentBean>) test.getDynamic(AllDynamicAttributesPersistent.dynamicComposedCollectionPropertyName);
		list.get(0).setDynamic(AllDynamicAttributesPersistent.colourPropertyName, "#000012");
		list.get(1).setDynamic(AllDynamicAttributesPersistent.colourPropertyName, "#000013");

		list = (List<PersistentBean>) test.getDynamic(AllDynamicAttributesPersistent.dynamicChildCollectionPropertyName);
		list.get(0).setDynamic(AllDynamicAttributesPersistent.colourPropertyName, "#000014");
		list.get(1).setDynamic(AllDynamicAttributesPersistent.colourPropertyName, "#000015");

		test = p.save(test);
		PersistentBean clone = p.retrieve(aadpd, test.getBizId());
		if (clone != test) {
			fail("save did not cache");
		}
		p.evictAllCached();
		
		clone = p.retrieve(aadpd, test.getBizId());
		assertNotNull(clone);
		if (clone == test) {
			fail("cache was not evicted");
		}
		
		assertEquals("#000001", clone.getDynamic(AllDynamicAttributesPersistent.colourPropertyName));
		assertEquals("#000002", ((Bean) clone.getDynamic(AllDynamicAttributesPersistent.aggregatedAssociationPropertyName)).getDynamic(AllDynamicAttributesPersistent.colourPropertyName));
		assertEquals("#000003", ((Bean) clone.getDynamic(AllDynamicAttributesPersistent.composedAssociationPropertyName)).getDynamic(AllDynamicAttributesPersistent.colourPropertyName));

		assertEquals("#000005", ((Bean) clone.getDynamic(AllDynamicAttributesPersistent.dynamicAggregatedAssociationPropertyName)).getDynamic(AllDynamicAttributesPersistent.colourPropertyName));
		assertEquals("#000006", ((Bean) clone.getDynamic(AllDynamicAttributesPersistent.dynamicComposedAssociationPropertyName)).getDynamic(AllDynamicAttributesPersistent.colourPropertyName));
		assertEquals("#000007", ((Bean) clone.getDynamic(AllDynamicAttributesPersistent.dynamicEmbeddedAssociationPropertyName)).getDynamic(AllDynamicAttributesPersistent.colourPropertyName));

		list = (List<PersistentBean>) clone.getDynamic(AllDynamicAttributesPersistent.composedCollectionPropertyName);
		assertEquals("#000010", list.get(0).getDynamic(AllDynamicAttributesPersistent.colourPropertyName));
		assertEquals("#000011", list.get(1).getDynamic(AllDynamicAttributesPersistent.colourPropertyName));
		
		list = (List<PersistentBean>) clone.getDynamic(AllDynamicAttributesPersistent.dynamicAggregatedCollectionPropertyName);
		assertEquals("#000010", list.get(0).getDynamic(AllDynamicAttributesPersistent.colourPropertyName));
		assertEquals("#000011", list.get(1).getDynamic(AllDynamicAttributesPersistent.colourPropertyName));
		
		list = (List<PersistentBean>) clone.getDynamic(AllDynamicAttributesPersistent.dynamicComposedCollectionPropertyName);
		assertEquals("#000012", list.get(0).getDynamic(AllDynamicAttributesPersistent.colourPropertyName));
		assertEquals("#000013", list.get(1).getDynamic(AllDynamicAttributesPersistent.colourPropertyName));

		List<ChildBean<Bean>> children = (List<ChildBean<Bean>>) clone.getDynamic(AllDynamicAttributesPersistent.dynamicChildCollectionPropertyName);
		assertEquals("#000014", children.get(0).getDynamic(AllDynamicAttributesPersistent.colourPropertyName));
		assertEquals("#000015", children.get(1).getDynamic(AllDynamicAttributesPersistent.colourPropertyName));
		assertSame(clone, children.get(0).getParent());
		assertSame(clone, children.get(1).getParent());

		PersistentBean cached = p.retrieve(aadpd, test.getBizId());
		if (clone != cached) {
			fail("populate did not cache");
		}
	}
	
	@Test
	@SuppressWarnings("unchecked")
	void testPersistAndPopulateACyclicDynamicDocument() throws Exception {
		PersistentBean test = Util.constructRandomInstance(u, m, aadpd, 2);

		PersistentBean bean = (PersistentBean) test.getDynamic(AllDynamicAttributesPersistent.dynamicAggregatedAssociationPropertyName);
		bean.setDynamic(AllDynamicAttributesPersistent.aggregatedAssociationPropertyName, test);

		List<PersistentBean> list = (List<PersistentBean>) test.getDynamic(AllDynamicAttributesPersistent.dynamicChildCollectionPropertyName);
		list.get(0).setDynamic(AllDynamicAttributesPersistent.colourPropertyName, "#000014");
		list.set(1, test);

		test = p.save(test);
		p.evictAllCached();
		test = p.retrieve(aadpd, test.getBizId());
		assertNotNull(test);
		bean = (PersistentBean) test.getDynamic(AllDynamicAttributesPersistent.dynamicAggregatedAssociationPropertyName);
		list = (List<PersistentBean>) test.getDynamic(AllDynamicAttributesPersistent.dynamicChildCollectionPropertyName);
		
		assertSame(test, bean.getDynamic(AllDynamicAttributesPersistent.aggregatedAssociationPropertyName));
		assertSame(test, list.get(1));
	}
	
	@Test
	void testDeletionOfDynamicDocument() throws Exception {
		PersistentBean test = Util.constructRandomInstance(u, m, aadpd, 3); // to get static -> dynamic -> static bean graph
		test = p.save(test);

		// Test
		assertEquals(2, p.newSQL("select count(1) from TEST_AllAttributesPersistent").scalarResult(Number.class).intValue());
		assertEquals(68, p.newSQL("select count(1) from TEST_AllDynamicAttributesPersistent").scalarResult(Number.class).intValue());
		assertEquals(188, p.newSQL("select count(1) from ADM_DynamicEntity").scalarResult(Number.class).intValue());
		assertEquals(165, p.newSQL("select count(1) from ADM_DynamicRelation").scalarResult(Number.class).intValue());
		
		p.delete(test);
		
		// Test
		assertEquals(2, p.newSQL("select count(1) from TEST_AllAttributesPersistent").scalarResult(Number.class).intValue());
		assertEquals(46, p.newSQL("select count(1) from TEST_AllDynamicAttributesPersistent").scalarResult(Number.class).intValue());
		assertEquals(122, p.newSQL("select count(1) from ADM_DynamicEntity").scalarResult(Number.class).intValue());
		assertEquals(72, p.newSQL("select count(1) from ADM_DynamicRelation").scalarResult(Number.class).intValue());
	}

	@Test
	@SuppressWarnings("java:S1854") // Persistence.save() may return a different instance; assignment captured intentionally
	void testConstraintViolationOnDeletionOfDynamicDocument() throws Exception {
		PersistentBean referenced = Util.constructRandomInstance(u, m, aadpd, 1);
		referenced = p.save(referenced);

		PersistentBean referrer = Util.constructRandomInstance(u, m, aadpd, 1);
		referrer.setDynamic(AllDynamicAttributesPersistent.dynamicComposedAssociationPropertyName, referenced);
		referrer = p.save(referrer);

		final PersistentBean toDelete = referenced;
		ReferentialConstraintViolationException rcve = assertThrows(ReferentialConstraintViolationException.class, () -> p.delete(toDelete));
		assertThat(rcve.getMessage(), is(notNullValue()));
	}
}
