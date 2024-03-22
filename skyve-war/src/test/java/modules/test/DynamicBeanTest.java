package modules.test;

import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.junit.Assert;
import org.junit.jupiter.api.Test;
import org.locationtech.jts.geom.Geometry;
import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.DynamicBean;
import org.skyve.domain.DynamicPersistentBean;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.types.DateOnly;
import org.skyve.domain.types.DateTime;
import org.skyve.domain.types.Decimal10;
import org.skyve.domain.types.Decimal2;
import org.skyve.domain.types.Decimal5;
import org.skyve.domain.types.TimeOnly;
import org.skyve.domain.types.Timestamp;
import org.skyve.impl.bind.BindUtil;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.DynamicImage;
import org.skyve.metadata.model.document.Relation;
import org.skyve.metadata.view.model.list.ListModel;
import org.skyve.util.BeanVisitor;
import org.skyve.util.Binder;
import org.skyve.util.JSON;
import org.skyve.util.Util;

import modules.admin.domain.Contact;
import modules.test.domain.AllAttributesPersistent;
import modules.test.domain.AllDynamicAttributesPersistent;

public class DynamicBeanTest extends AbstractSkyveTest {
	public static final String ONE = "one";
	public static final String TWO = "two";
	public static final String THREE = "three";
	
	@Test
	@SuppressWarnings("static-method")
	public void testAbstractBean() {
		Contact contact = Contact.newInstance();
		contact.putDynamic(ONE, TWO);
		Assert.assertEquals("Dynamic Property set correctly", TWO, contact.getDynamic(ONE));
		Assert.assertTrue("Is a dynamic property", contact.isDynamic(ONE));
		contact.setDynamic(ONE, THREE);
		Assert.assertEquals("Dynamic Property set correctly", THREE, contact.getDynamic(ONE));
		try {
			contact.getDynamic(THREE);
			Assert.fail("Not a property");
		}
		catch (@SuppressWarnings("unused") Exception e) {
			// do nothing
		}
		Assert.assertFalse("Not a property", contact.isDynamic(THREE));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testDynamicBean() {
		Map<String, Object> values = new TreeMap<>();
		values.put(ONE, TWO);
		DynamicBean contact = new DynamicBean(Contact.MODULE_NAME, Contact.DOCUMENT_NAME, values);
		Assert.assertEquals("Dynamic Property set correctly", TWO, contact.getDynamic(ONE));
		Assert.assertTrue("Is a dynamic property", contact.isDynamic(ONE));
		contact.setDynamic(ONE, THREE);
		Assert.assertEquals("Dynamic Property set correctly", THREE, contact.getDynamic(ONE));
	}
	
	@Test
	public void testGetAllAttributesPersistentClass() throws Exception {
		Assert.assertEquals("ADAPD document should create AllDynamicAtrributesPersistent", AllDynamicAttributesPersistent.class, adapd.newInstance(u).getClass());
	}

	@Test
	public void testGetAllAttributesDynamicPersistentClass() throws Exception {
		Assert.assertEquals("AADPD document should create AllAtrributesDynamicPersistent", DynamicPersistentBean.class, aadpd.newInstance(u).getClass());
	}
	
	@Test
	public void testDefaultValues() throws Exception {
		Bean bean = aadpd.newInstance(u);
		Assert.assertEquals("Boolean default value", Boolean.TRUE, Binder.get(bean, AllAttributesPersistent.booleanFlagPropertyName));
		Assert.assertEquals("Colour default value", "#000000", Binder.get(bean, AllAttributesPersistent.colourPropertyName));
		Assert.assertEquals("Date default value", new DateOnly("2021-10-21"), Binder.get(bean, AllAttributesPersistent.datePropertyName));
		Assert.assertEquals("DateTime default value", new DateTime("2021-10-21T07:48:29Z"), Binder.get(bean, AllAttributesPersistent.dateTimePropertyName));
		Assert.assertEquals("Decimal10 default value", new Decimal10("100.1234567899"), Binder.get(bean, AllAttributesPersistent.decimal10PropertyName));
		Assert.assertEquals("Decimal2 default value", new Decimal2("100.12"), Binder.get(bean, AllAttributesPersistent.decimal2PropertyName));
		Assert.assertEquals("Decimal5 default value", new Decimal5("100.12345"), Binder.get(bean, AllAttributesPersistent.decimal5PropertyName));
		Assert.assertEquals("Enum default value", "one", Binder.get(bean, AllAttributesPersistent.enum3PropertyName));
		Geometry g = (Geometry) Binder.fromSerialised(Geometry.class, "POINT(0 0)");
		Assert.assertEquals("Geometry default value", g, Binder.get(bean, AllAttributesPersistent.geometryPropertyName));
		Assert.assertEquals("Id default value", "1234567890", Binder.get(bean, AllAttributesPersistent.idPropertyName));
		Assert.assertEquals("Integer default value", Integer.valueOf(123), Binder.get(bean, AllAttributesPersistent.normalIntegerPropertyName));
		Assert.assertEquals("Long integer default value", Long.valueOf(123), Binder.get(bean, AllAttributesPersistent.longIntegerPropertyName));
		Assert.assertEquals("Markup default value", "<h1>Markup</h1>", Binder.get(bean, AllAttributesPersistent.markupPropertyName));
		Assert.assertEquals("Memo default value", "Memo", Binder.get(bean, AllAttributesPersistent.memoPropertyName));
		Assert.assertEquals("Text default value", "Text", Binder.get(bean, AllAttributesPersistent.textPropertyName));
		Assert.assertEquals("Time default value", new TimeOnly("07:51:26"), Binder.get(bean, AllAttributesPersistent.timePropertyName));
		Assert.assertEquals("Timestamp default value", new Timestamp("2021-10-21T07:48:29Z"), Binder.get(bean, AllAttributesPersistent.timestampPropertyName));
		
		bean = adapd.newInstance(u);
		Assert.assertEquals("Colour default value", "#000000", Binder.get(bean, AllAttributesPersistent.colourPropertyName));
	}
	
	@Test
	public void testJSON() throws Exception {
		Bean bean = Util.constructRandomInstance(u, m, aadpd, 2);
		String bizId = bean.getBizId();
		String json = JSON.marshall(c, bean);
		bean = (Bean) JSON.unmarshall(u, json);
		Assert.assertEquals("JSON marshall/unmarshall problem", bizId, bean.getBizId());
		Assert.assertEquals("JSON unmarshall document should create AllAtrributesDynamicPersistent", DynamicPersistentBean.class, bean.getClass());

		bean = Util.constructRandomInstance(u, m, adapd, 2);
		bizId = bean.getBizId();
		json = JSON.marshall(c, bean);
		bean = (Bean) JSON.unmarshall(u, json);
		Assert.assertEquals("JSON marshall/unmarshall problem", bizId, bean.getBizId());
		Assert.assertEquals("JSON unmarshall document should create AllAtrributesPersistent", AllDynamicAttributesPersistent.class, bean.getClass());
	}
	
	@Test
	public void testBinderWithDynamicAttributes() throws Exception {
		Bean bean = Util.constructRandomInstance(u, m, adapd, 2);
		AllDynamicAttributesPersistent testAssignment = AllDynamicAttributesPersistent.newInstance();
		testBinder(bean, AllDynamicAttributesPersistent.aggregatedAssociationPropertyName, AllDynamicAttributesPersistent.composedCollectionPropertyName, testAssignment);
	}

	@Test
	public void testBinderWithDynamicDocument() throws Exception {
		Bean bean = Util.constructRandomInstance(u, m, aadpd, 2);
		AllAttributesPersistent testAssignment = AllAttributesPersistent.newInstance();
		testBinder(bean, AllAttributesPersistent.aggregatedAssociationPropertyName, AllAttributesPersistent.aggregatedCollectionPropertyName, testAssignment);
	}

	private static final Integer INTEGER = Integer.valueOf(Integer.MAX_VALUE);
	
	private static void testBinder(Bean bean, String associationPropertyName, String collectionPropertyName, PersistentBean testAssignment) {
		// Simple scalar
		Binder.set(bean, AllAttributesPersistent.booleanFlagPropertyName, Boolean.TRUE);
		Assert.assertEquals(Boolean.TRUE, Binder.get(bean, AllAttributesPersistent.booleanFlagPropertyName));
		Binder.set(bean, AllAttributesPersistent.normalIntegerPropertyName, INTEGER);
		Assert.assertEquals(INTEGER, Binder.get(bean, AllAttributesPersistent.normalIntegerPropertyName));

		// Simple association
		Binder.set(bean, associationPropertyName, bean);
		Assert.assertEquals(bean, Binder.get(bean, associationPropertyName));

		// Simple collection
		Assert.assertTrue(Binder.get(bean, collectionPropertyName) instanceof List<?>);
		
		// Compound association to scalar
		String binding = Binder.createCompoundBinding(associationPropertyName, AllAttributesPersistent.normalIntegerPropertyName);
		Binder.set(bean, binding, INTEGER);
		Assert.assertEquals(INTEGER, Binder.get(bean, binding));
		
		// Compound association to association
		binding = Binder.createCompoundBinding(associationPropertyName, associationPropertyName);
		Object oldValue = Binder.get(bean, binding);
		Binder.set(bean, binding, bean);
		Assert.assertEquals(bean, Binder.get(bean, binding));
		Binder.set(bean, binding, oldValue);

		// Compound association to collection
		binding = Binder.createCompoundBinding(associationPropertyName, collectionPropertyName);
		Assert.assertTrue(Binder.get(bean, binding) instanceof List<?>);

		// Compound association to indexed
		binding = Binder.createIndexedBinding(Binder.createCompoundBinding(associationPropertyName, collectionPropertyName), 0);
		Binder.set(bean, binding, testAssignment);
		Assert.assertEquals(testAssignment, Binder.get(bean, binding));

		// Check set above via Compound association to Id
		binding = Binder.createIdBinding(Binder.createCompoundBinding(associationPropertyName, collectionPropertyName), testAssignment.getBizId());
		Assert.assertEquals(testAssignment, Binder.get(bean, binding));
		
		// Indexed collection to scalar
		binding = Binder.createCompoundBinding(Binder.createIndexedBinding(collectionPropertyName, 0), AllAttributesPersistent.normalIntegerPropertyName);
		Binder.set(bean, binding, INTEGER);
		Assert.assertEquals(INTEGER, Binder.get(bean, binding));

		// Id collection to scalar
		@SuppressWarnings("unchecked")
		List<? extends Bean> elements = (List<? extends Bean>) Binder.get(bean, collectionPropertyName);
		// its a test - let in NPE
		@SuppressWarnings("null")
		String bizId = elements.get(1).getBizId();
		binding = Binder.createCompoundBinding(Binder.createIdBinding(collectionPropertyName, bizId), AllAttributesPersistent.normalIntegerPropertyName);
		Binder.set(bean, binding, INTEGER);
		Assert.assertEquals(INTEGER, Binder.get(bean, binding));
	}
	
	@Test
	public void testBeanVisitorWithDynamicAttributes() throws Exception {
		Bean bean = Util.constructRandomInstance(u, m, adapd, 2);
		testBeanVisitor(adapd, bean);
	}

	@Test
	public void testBeanVisitorWithDynamicDocument() throws Exception {
		Bean bean = Util.constructRandomInstance(u, m, aadpd, 2);
		testBeanVisitor(aadpd, bean);
	}

	private void testBeanVisitor(Document document, Bean bean) throws Exception {
		new BeanVisitor(false, false, false) {
			@Override
			protected boolean accept(String binding, Document visitedDocument, Document owningDocument, Relation owningRelation, Bean visitedBean) throws Exception {
				return true;
			}
		}.visit(document, bean, c);

		new BeanVisitor(false, true, false) {
			@Override
			protected boolean accept(String binding, Document visitedDocument, Document owningDocument, Relation owningRelation, Bean visitedBean) throws Exception {
				return true;
			}
		}.visit(document, bean, c);
	}
	
	@Test
	public void testDynamicDefinitions() throws Exception {
		Bizlet<Bean> bizlet = aadpd.getBizlet(c);
		Assert.assertNotNull(bizlet);
		
		Object dataFactory = CORE.getRepository().getDataFactory(c, aadpd);
		Assert.assertNotNull(dataFactory);
		
		ServerSideAction<Bean> action = aadpd.getServerSideAction(c, "ServerSideAction", true);
		Assert.assertNotNull(action);

		DynamicImage<Bean> image = aadpd.getDynamicImage(c, "DynamicImage");
		Assert.assertNotNull(image);

		ListModel<Bean> model = aadpd.getListModel(c, "ListModel", true);
		Assert.assertNotNull(model);
	}
	
	@Test
	public void testDynamicInverses() throws Exception {
		// added to inverse collection assigns to other side (dynamic/static)
		DynamicPersistentBean dynamicBean = aadpd.newInstance(u);
		AllDynamicAttributesPersistent staticBean = adapd.newInstance(u);
		BindUtil.addElementToCollection(dynamicBean, AllDynamicAttributesPersistent.dynamicInverseAggregatedAssociationPropertyName, staticBean);
		Assert.assertSame(dynamicBean, staticBean.getDynamic(AllDynamicAttributesPersistent.dynamicAggregatedAssociationPropertyName));

		// removing from inverse collection nulls other side (dynamic/static)
		BindUtil.removeElementFromCollection(dynamicBean, AllDynamicAttributesPersistent.dynamicInverseAggregatedAssociationPropertyName, staticBean);
		Assert.assertNull(staticBean.getDynamic(AllDynamicAttributesPersistent.dynamicAggregatedAssociationPropertyName));

		// added to inverse collection assigns to other side (static/dynamic)
		staticBean = adapd.newInstance(u);
		dynamicBean = aadpd.newInstance(u);
		BindUtil.addElementToCollection(staticBean, AllDynamicAttributesPersistent.dynamicInverseAggregatedAssociationPropertyName, dynamicBean);
		Assert.assertSame(staticBean, dynamicBean.getDynamic(AllDynamicAttributesPersistent.dynamicAggregatedAssociationPropertyName));

		// removing from inverse collection nulls other side (static/dynamic)
		BindUtil.removeElementFromCollection(staticBean, AllDynamicAttributesPersistent.dynamicInverseAggregatedAssociationPropertyName, dynamicBean);
		Assert.assertNull(dynamicBean.getDynamic(AllDynamicAttributesPersistent.dynamicAggregatedAssociationPropertyName));

		// setting dynamic association adds to inverse collection
		dynamicBean = aadpd.newInstance(u);
		DynamicPersistentBean nutherDynamicBean = aadpd.newInstance(u);
		BindUtil.setAssociation(dynamicBean, AllDynamicAttributesPersistent.dynamicAggregatedAssociationPropertyName, nutherDynamicBean);
		@SuppressWarnings("unchecked")
		List<Bean> list = (List<Bean>) nutherDynamicBean.getDynamic(AllDynamicAttributesPersistent.dynamicInverseAggregatedAssociationPropertyName);
		Assert.assertTrue(list.contains(dynamicBean));
		
		// overwriting dynamic association removes from old inverse collection and adds to new inverse collection
		DynamicPersistentBean nutherNutherDynamicBean = aadpd.newInstance(u);
		BindUtil.setAssociation(dynamicBean, AllDynamicAttributesPersistent.dynamicAggregatedAssociationPropertyName, nutherNutherDynamicBean);
		//old
		Assert.assertFalse(list.contains(dynamicBean));
		// new
		@SuppressWarnings("unchecked")
		List<Bean> newList = (List<Bean>) nutherNutherDynamicBean.getDynamic(AllDynamicAttributesPersistent.dynamicInverseAggregatedAssociationPropertyName);
		Assert.assertTrue(newList.contains(dynamicBean));
	}
}
