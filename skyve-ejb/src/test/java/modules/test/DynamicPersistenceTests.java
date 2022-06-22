package modules.test;

import java.util.List;

import org.junit.Assert;
import org.junit.Test;
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

public class DynamicPersistenceTests extends AbstractSkyveTestDispose {
	@Test
	public void testHasDynamic() throws Exception {
		Assert.assertFalse(aapd.hasDynamic());
		Assert.assertTrue(adapd.hasDynamic()); // has dynamic attributes
		Assert.assertTrue(aadpd.hasDynamic()); // is dynamic
		Assert.assertFalse(aarpd.hasDynamic());
		Assert.assertFalse(ad1.hasDynamic());
		Assert.assertFalse(ad2.hasDynamic());
		Assert.assertFalse(ao2m.hasDynamic());
		Assert.assertFalse(ao2o.hasDynamic());
		Assert.assertTrue(dmed.hasDynamic()); // is dynamic
		Assert.assertTrue(dmsd.hasDynamic()); // is dynamic
		Assert.assertFalse(hd.hasDynamic());
		Assert.assertFalse(im2mpd.hasDynamic());
		Assert.assertFalse(io2mpd.hasDynamic());
		Assert.assertFalse(io2opd.hasDynamic());
		Assert.assertTrue(mbd.hasDynamic()); // points to messd (dynamic)
		Assert.assertTrue(mejsd.hasDynamic()); // has dynamic attribute
		Assert.assertTrue(messd.hasDynamic()); // has dynamic attribute
		Assert.assertTrue(msjsd.hasDynamic()); // has dynamic attribute
		Assert.assertTrue(msssd.hasDynamic()); // has dynamic attribute
		Assert.assertFalse(rd.hasDynamic());
		Assert.assertFalse(ucn.hasDynamic());
		Assert.assertFalse(ucnn.hasDynamic());
	}

	@Test
	@SuppressWarnings("unchecked")
	public void testPersistenceOfDynamicAttributes() throws Exception {
		AllDynamicAttributesPersistent test = Util.constructRandomInstance(u, m, adapd, 2);
		test = p.save(test);
		test = p.save(test);

		Assert.assertEquals(Integer.valueOf(0), test.getBizVersion());
		Assert.assertEquals(Integer.valueOf(0), test.getAggregatedAssociation().getBizVersion());
		Assert.assertEquals(Integer.valueOf(0), test.getComposedAssociation().getBizVersion());
		Assert.assertEquals(null, test.getEmbeddedAssociation().getBizVersion());
		Assert.assertEquals(Integer.valueOf(0), ((DynamicPersistentBean) test.getDynamic(AllDynamicAttributesPersistent.dynamicAggregatedAssociationPropertyName)).getBizVersion());
		Assert.assertEquals(Integer.valueOf(0), ((DynamicPersistentBean) test.getDynamic(AllDynamicAttributesPersistent.dynamicComposedAssociationPropertyName)).getBizVersion());
		Assert.assertEquals(Integer.valueOf(0), ((DynamicPersistentBean) test.getDynamic(AllDynamicAttributesPersistent.dynamicEmbeddedAssociationPropertyName)).getBizVersion());
		Assert.assertEquals(Integer.valueOf(0), test.getComposedCollection().get(0).getBizVersion());
		Assert.assertEquals(Integer.valueOf(0), test.getComposedCollection().get(1).getBizVersion());
		List<DynamicPersistentBean> list = (List<DynamicPersistentBean>) test.getDynamic(AllDynamicAttributesPersistent.dynamicAggregatedCollectionPropertyName);
		Assert.assertEquals(Integer.valueOf(0), list.get(0).getBizVersion());
		Assert.assertEquals(Integer.valueOf(0), list.get(1).getBizVersion());
		list = (List<DynamicPersistentBean>) test.getDynamic(AllDynamicAttributesPersistent.dynamicComposedCollectionPropertyName);
		Assert.assertEquals(Integer.valueOf(0), list.get(0).getBizVersion());
		Assert.assertEquals(Integer.valueOf(0), list.get(1).getBizVersion());
		list = (List<DynamicPersistentBean>) test.getDynamic(AllDynamicAttributesPersistent.dynamicChildCollectionPropertyName);
		Assert.assertEquals(Integer.valueOf(0), list.get(0).getBizVersion());
		Assert.assertEquals(Integer.valueOf(0), list.get(1).getBizVersion());
	}

	@Test
	public void testCoercionOfDynamicAttributes() throws Exception {
		AllDynamicAttributesPersistent test = Util.constructRandomInstance(u, m, adapd, 1);
		test.setDynamic(AllDynamicAttributesPersistent.enum3PropertyName, "one");
		test = p.save(test);
		
		p.evictAllCached();
		test = p.retrieve(adapd, test.getBizId());
		
		Assert.assertTrue(test.getDynamic(AllDynamicAttributesPersistent.booleanFlagPropertyName) instanceof Boolean);
		Assert.assertTrue(test.getDynamic(AllDynamicAttributesPersistent.datePropertyName) instanceof DateOnly);
		Assert.assertTrue(test.getDynamic(AllDynamicAttributesPersistent.dateTimePropertyName) instanceof DateTime);
		Assert.assertTrue(test.getDynamic(AllDynamicAttributesPersistent.decimal10PropertyName) instanceof Decimal10);
		Assert.assertTrue(test.getDynamic(AllDynamicAttributesPersistent.decimal2PropertyName) instanceof Decimal2);
		Assert.assertTrue(test.getDynamic(AllDynamicAttributesPersistent.decimal5PropertyName) instanceof Decimal5);
		Assert.assertTrue(test.getDynamic(AllDynamicAttributesPersistent.enum3PropertyName) instanceof String);
		Assert.assertTrue(test.getDynamic(AllDynamicAttributesPersistent.geometryPropertyName) instanceof Geometry);
		Assert.assertTrue(test.getDynamic(AllDynamicAttributesPersistent.longIntegerPropertyName) instanceof Long);
		Assert.assertTrue(test.getDynamic(AllDynamicAttributesPersistent.normalIntegerPropertyName) instanceof Integer);
		Assert.assertTrue(test.getDynamic(AllDynamicAttributesPersistent.timePropertyName) instanceof TimeOnly);
		Assert.assertTrue(test.getDynamic(AllDynamicAttributesPersistent.timestampPropertyName) instanceof Timestamp);
	}
	
	@Test
	@SuppressWarnings("unchecked")
	public void testRetrievalOfDynamicAttributes() throws Exception {
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
		
		Assert.assertEquals("#000001", clone.getDynamic(AllDynamicAttributesPersistent.colourPropertyName));
		Assert.assertEquals("#000002", clone.getAggregatedAssociation().getDynamic(AllDynamicAttributesPersistent.colourPropertyName));
		Assert.assertEquals("#000003", clone.getComposedAssociation().getDynamic(AllDynamicAttributesPersistent.colourPropertyName));
		Assert.assertEquals("#000004", clone.getEmbeddedAssociation().getColour());

		Assert.assertEquals("#000005", ((Bean) clone.getDynamic(AllDynamicAttributesPersistent.dynamicAggregatedAssociationPropertyName)).getDynamic(AllDynamicAttributesPersistent.colourPropertyName));
		Assert.assertEquals("#000006", ((Bean) clone.getDynamic(AllDynamicAttributesPersistent.dynamicComposedAssociationPropertyName)).getDynamic(AllDynamicAttributesPersistent.colourPropertyName));
		Assert.assertEquals("#000007", ((Bean) clone.getDynamic(AllDynamicAttributesPersistent.dynamicEmbeddedAssociationPropertyName)).getDynamic(AllDynamicAttributesPersistent.colourPropertyName));
		
		Assert.assertEquals("#000008", clone.getComposedCollection().get(0).getDynamic(AllDynamicAttributesPersistent.colourPropertyName));
		Assert.assertEquals("#000009", clone.getComposedCollection().get(1).getDynamic(AllDynamicAttributesPersistent.colourPropertyName));
		
		list = (List<PersistentBean>) clone.getDynamic(AllDynamicAttributesPersistent.dynamicAggregatedCollectionPropertyName);
		Assert.assertEquals("#000010", list.get(0).getDynamic(AllDynamicAttributesPersistent.colourPropertyName));
		Assert.assertEquals("#000011", list.get(1).getDynamic(AllDynamicAttributesPersistent.colourPropertyName));
		
		list = (List<PersistentBean>) clone.getDynamic(AllDynamicAttributesPersistent.dynamicComposedCollectionPropertyName);
		Assert.assertEquals("#000012", list.get(0).getDynamic(AllDynamicAttributesPersistent.colourPropertyName));
		Assert.assertEquals("#000013", list.get(1).getDynamic(AllDynamicAttributesPersistent.colourPropertyName));

		List<ChildBean<Bean>> children = (List<ChildBean<Bean>>) clone.getDynamic(AllDynamicAttributesPersistent.dynamicChildCollectionPropertyName);
		Assert.assertEquals("#000014", children.get(0).getDynamic(AllDynamicAttributesPersistent.colourPropertyName));
		Assert.assertEquals("#000015", children.get(1).getDynamic(AllDynamicAttributesPersistent.colourPropertyName));
		Assert.assertSame(clone, children.get(0).getParent());
		Assert.assertSame(clone, children.get(1).getParent());
	}

	@Test
	public void testDeletionOfDynamicAttributes() throws Exception {
		AllDynamicAttributesPersistent test = Util.constructRandomInstance(u, m, adapd, 3); // to get static -> dynamic -> static bean graph
		test = p.save(test);

		// Test
		Assert.assertEquals(6, p.newSQL("select count(1) from TEST_AllAttributesPersistent").scalarResult(Number.class).intValue());
		Assert.assertEquals(59, p.newSQL("select count(1) from TEST_AllDynamicAttributesPersistent").scalarResult(Number.class).intValue());
		Assert.assertEquals(160, p.newSQL("select count(1) from ADM_DynamicEntity").scalarResult(Number.class).intValue());
		Assert.assertEquals(141, p.newSQL("select count(1) from ADM_DynamicRelation").scalarResult(Number.class).intValue());
		
		p.delete(test);
		
		// Test
		Assert.assertEquals(5, p.newSQL("select count(1) from TEST_AllAttributesPersistent").scalarResult(Number.class).intValue());
		Assert.assertEquals(36, p.newSQL("select count(1) from TEST_AllDynamicAttributesPersistent").scalarResult(Number.class).intValue());
		Assert.assertEquals(94, p.newSQL("select count(1) from ADM_DynamicEntity").scalarResult(Number.class).intValue());
		Assert.assertEquals(54, p.newSQL("select count(1) from ADM_DynamicRelation").scalarResult(Number.class).intValue());
	}
	
	@Test(expected = ReferentialConstraintViolationException.class)
	public void testConstraintViolationOnDeletionOfDynamicAttributes() throws Exception {
		PersistentBean referenced = Util.constructRandomInstance(u, m, adapd, 1);
		referenced = p.save(referenced);

		PersistentBean referrer = Util.constructRandomInstance(u, m, aadpd, 1);
		referrer.setDynamic(AllDynamicAttributesPersistent.dynamicComposedAssociationPropertyName, referenced);
		referrer = p.save(referrer);
		
		p.delete(referenced);
	}

	
	@Test
	@SuppressWarnings("unchecked")
	public void testPersistenceOfDynamicDocument() throws Exception {
		PersistentBean test = Util.constructRandomInstance(u, m, aadpd, 2);
		test = p.save(test);
		test = p.save(test);

		Assert.assertEquals(Integer.valueOf(0), test.getBizVersion());
		Assert.assertEquals(Integer.valueOf(0), ((PersistentBean) test.getDynamic(AllDynamicAttributesPersistent.aggregatedAssociationPropertyName)).getBizVersion());
		Assert.assertEquals(Integer.valueOf(0), ((PersistentBean) test.getDynamic(AllDynamicAttributesPersistent.composedAssociationPropertyName)).getBizVersion());
		Assert.assertEquals(Integer.valueOf(0), ((DynamicPersistentBean) test.getDynamic(AllDynamicAttributesPersistent.dynamicAggregatedAssociationPropertyName)).getBizVersion());
		Assert.assertEquals(Integer.valueOf(0), ((DynamicPersistentBean) test.getDynamic(AllDynamicAttributesPersistent.dynamicComposedAssociationPropertyName)).getBizVersion());
		Assert.assertEquals(Integer.valueOf(0), ((DynamicPersistentBean) test.getDynamic(AllDynamicAttributesPersistent.dynamicEmbeddedAssociationPropertyName)).getBizVersion());

		List<PersistentBean> list = (List<PersistentBean>) test.getDynamic(AllDynamicAttributesPersistent.composedCollectionPropertyName);
		Assert.assertEquals(Integer.valueOf(0), list.get(0).getBizVersion());
		Assert.assertEquals(Integer.valueOf(0), list.get(1).getBizVersion());
		list = (List<PersistentBean>) test.getDynamic(AllDynamicAttributesPersistent.dynamicAggregatedCollectionPropertyName);
		Assert.assertEquals(Integer.valueOf(0), list.get(0).getBizVersion());
		Assert.assertEquals(Integer.valueOf(0), list.get(1).getBizVersion());
		list = (List<PersistentBean>) test.getDynamic(AllDynamicAttributesPersistent.dynamicComposedCollectionPropertyName);
		Assert.assertEquals(Integer.valueOf(0), list.get(0).getBizVersion());
		Assert.assertEquals(Integer.valueOf(0), list.get(1).getBizVersion());
		list = (List<PersistentBean>) test.getDynamic(AllDynamicAttributesPersistent.dynamicChildCollectionPropertyName);
		Assert.assertEquals(Integer.valueOf(0), list.get(0).getBizVersion());
		Assert.assertEquals(Integer.valueOf(0), list.get(1).getBizVersion());
	}

	@Test
	@SuppressWarnings("unchecked")
	public void testRetrievalOfDynamicDocument() throws Exception {
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
			Assert.fail("save did not cache");
		}
		p.evictAllCached();
		
		clone = p.retrieve(aadpd, test.getBizId());
		if (clone == test) {
			Assert.fail("cache was not evicted");
		}
		
		Assert.assertEquals("#000001", clone.getDynamic(AllDynamicAttributesPersistent.colourPropertyName));
		Assert.assertEquals("#000002", ((Bean) clone.getDynamic(AllDynamicAttributesPersistent.aggregatedAssociationPropertyName)).getDynamic(AllDynamicAttributesPersistent.colourPropertyName));
		Assert.assertEquals("#000003", ((Bean) clone.getDynamic(AllDynamicAttributesPersistent.composedAssociationPropertyName)).getDynamic(AllDynamicAttributesPersistent.colourPropertyName));

		Assert.assertEquals("#000005", ((Bean) clone.getDynamic(AllDynamicAttributesPersistent.dynamicAggregatedAssociationPropertyName)).getDynamic(AllDynamicAttributesPersistent.colourPropertyName));
		Assert.assertEquals("#000006", ((Bean) clone.getDynamic(AllDynamicAttributesPersistent.dynamicComposedAssociationPropertyName)).getDynamic(AllDynamicAttributesPersistent.colourPropertyName));
		Assert.assertEquals("#000007", ((Bean) clone.getDynamic(AllDynamicAttributesPersistent.dynamicEmbeddedAssociationPropertyName)).getDynamic(AllDynamicAttributesPersistent.colourPropertyName));

		list = (List<PersistentBean>) clone.getDynamic(AllDynamicAttributesPersistent.composedCollectionPropertyName);
		Assert.assertEquals("#000010", list.get(0).getDynamic(AllDynamicAttributesPersistent.colourPropertyName));
		Assert.assertEquals("#000011", list.get(1).getDynamic(AllDynamicAttributesPersistent.colourPropertyName));
		
		list = (List<PersistentBean>) clone.getDynamic(AllDynamicAttributesPersistent.dynamicAggregatedCollectionPropertyName);
		Assert.assertEquals("#000010", list.get(0).getDynamic(AllDynamicAttributesPersistent.colourPropertyName));
		Assert.assertEquals("#000011", list.get(1).getDynamic(AllDynamicAttributesPersistent.colourPropertyName));
		
		list = (List<PersistentBean>) clone.getDynamic(AllDynamicAttributesPersistent.dynamicComposedCollectionPropertyName);
		Assert.assertEquals("#000012", list.get(0).getDynamic(AllDynamicAttributesPersistent.colourPropertyName));
		Assert.assertEquals("#000013", list.get(1).getDynamic(AllDynamicAttributesPersistent.colourPropertyName));

		List<ChildBean<Bean>> children = (List<ChildBean<Bean>>) clone.getDynamic(AllDynamicAttributesPersistent.dynamicChildCollectionPropertyName);
		Assert.assertEquals("#000014", children.get(0).getDynamic(AllDynamicAttributesPersistent.colourPropertyName));
		Assert.assertEquals("#000015", children.get(1).getDynamic(AllDynamicAttributesPersistent.colourPropertyName));
		Assert.assertSame(clone, children.get(0).getParent());
		Assert.assertSame(clone, children.get(1).getParent());

		PersistentBean cached = p.retrieve(aadpd, test.getBizId());
		if (clone != cached) {
			Assert.fail("populate did not cache");
		}
	}
	
	@Test
	@SuppressWarnings("unchecked")
	public void testPersistAndPopulateACyclicDynamicDocument() throws Exception {
		PersistentBean test = Util.constructRandomInstance(u, m, aadpd, 2);

		PersistentBean bean = (PersistentBean) test.getDynamic(AllDynamicAttributesPersistent.dynamicAggregatedAssociationPropertyName);
		bean.setDynamic(AllDynamicAttributesPersistent.aggregatedAssociationPropertyName, test);

		List<PersistentBean> list = (List<PersistentBean>) test.getDynamic(AllDynamicAttributesPersistent.dynamicChildCollectionPropertyName);
		list.get(0).setDynamic(AllDynamicAttributesPersistent.colourPropertyName, "#000014");
		list.set(1, test);

		test = p.save(test);
		p.evictAllCached();
		test = p.retrieve(aadpd, test.getBizId());
		bean = (PersistentBean) test.getDynamic(AllDynamicAttributesPersistent.dynamicAggregatedAssociationPropertyName);
		list = (List<PersistentBean>) test.getDynamic(AllDynamicAttributesPersistent.dynamicChildCollectionPropertyName);
		
		Assert.assertSame(test, bean.getDynamic(AllDynamicAttributesPersistent.aggregatedAssociationPropertyName));
		Assert.assertSame(test, list.get(1));
	}
	
	@Test
	public void testDeletionOfDynamicDocument() throws Exception {
		PersistentBean test = Util.constructRandomInstance(u, m, aadpd, 3); // to get static -> dynamic -> static bean graph
		test = p.save(test);

		// Test
		Assert.assertEquals(2, p.newSQL("select count(1) from TEST_AllAttributesPersistent").scalarResult(Number.class).intValue());
		Assert.assertEquals(68, p.newSQL("select count(1) from TEST_AllDynamicAttributesPersistent").scalarResult(Number.class).intValue());
		Assert.assertEquals(188, p.newSQL("select count(1) from ADM_DynamicEntity").scalarResult(Number.class).intValue());
		Assert.assertEquals(165, p.newSQL("select count(1) from ADM_DynamicRelation").scalarResult(Number.class).intValue());
		
		p.delete(test);
		
		// Test
		Assert.assertEquals(2, p.newSQL("select count(1) from TEST_AllAttributesPersistent").scalarResult(Number.class).intValue());
		Assert.assertEquals(46, p.newSQL("select count(1) from TEST_AllDynamicAttributesPersistent").scalarResult(Number.class).intValue());
		Assert.assertEquals(122, p.newSQL("select count(1) from ADM_DynamicEntity").scalarResult(Number.class).intValue());
		Assert.assertEquals(72, p.newSQL("select count(1) from ADM_DynamicRelation").scalarResult(Number.class).intValue());
	}

	@Test(expected = ReferentialConstraintViolationException.class)
	public void testConstraintViolationOnDeletionOfDynamicDocument() throws Exception {
		PersistentBean referenced = Util.constructRandomInstance(u, m, aadpd, 1);
		referenced = p.save(referenced);

		PersistentBean referrer = Util.constructRandomInstance(u, m, aadpd, 1);
		referrer.setDynamic(AllDynamicAttributesPersistent.dynamicComposedAssociationPropertyName, referenced);
		referrer = p.save(referrer);
		
		p.delete(referenced);
	}
}
