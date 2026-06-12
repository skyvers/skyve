package modules.test;

import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.TreeMap;

import org.junit.jupiter.api.Assertions;
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
import modules.test.AllAttributesDynamicPersistent.TestDynamicImage;
import modules.test.AllAttributesDynamicPersistent.TestListModel;
import modules.test.AllAttributesDynamicPersistent.TestServerSideAction;
import modules.test.domain.AllAttributesPersistent;
import modules.test.domain.AllDynamicAttributesPersistent;

class DynamicBeanTest extends AbstractSkyveTest {
	public static final String ONE = "one";
	public static final String TWO = "two";
	public static final String THREE = "three";
	
	@Test
	@SuppressWarnings("static-method")
	void testAbstractBean() {
		Contact contact = Contact.newInstance();
		contact.putDynamic(ONE, TWO);
		Assertions.assertEquals(TWO, contact.getDynamic(ONE), "Dynamic Property set correctly");
		Assertions.assertTrue(contact.isDynamic(ONE), "Is a dynamic property");
		contact.setDynamic(ONE, THREE);
		Assertions.assertEquals(THREE, contact.getDynamic(ONE), "Dynamic Property set correctly");
		try {
			contact.getDynamic(THREE);
			Assertions.fail("Not a property");
		}
		catch (@SuppressWarnings("unused") Exception e) {
			// do nothing
		}
		Assertions.assertFalse(contact.isDynamic(THREE), "Not a property");
	}

	@Test
	@SuppressWarnings("static-method")
	void testDynamicBean() {
		Map<String, Object> values = new TreeMap<>();
		values.put(ONE, TWO);
		DynamicBean contact = new DynamicBean(Contact.MODULE_NAME, Contact.DOCUMENT_NAME, values);
		Assertions.assertEquals(TWO, contact.getDynamic(ONE), "Dynamic Property set correctly");
		Assertions.assertTrue(contact.isDynamic(ONE), "Is a dynamic property");
		contact.setDynamic(ONE, THREE);
		Assertions.assertEquals(THREE, contact.getDynamic(ONE), "Dynamic Property set correctly");
	}
	
	@Test
	void testGetAllAttributesPersistentClass() throws Exception {
		Assertions.assertEquals(AllDynamicAttributesPersistent.class, adapd.newInstance(u).getClass(), "ADAPD document should create AllDynamicAtrributesPersistent");
	}

	@Test
	void testGetAllAttributesDynamicPersistentClass() throws Exception {
		Assertions.assertEquals(DynamicPersistentBean.class, aadpd.newInstance(u).getClass(), "AADPD document should create AllAtrributesDynamicPersistent");
	}
	
	@Test
	void testDefaultValues() throws Exception {
		Bean bean = aadpd.newInstance(u);
		Assertions.assertEquals(Boolean.TRUE, Binder.get(bean, AllAttributesPersistent.booleanFlagPropertyName), "Boolean default value");
		Assertions.assertEquals("#000000", Binder.get(bean, AllAttributesPersistent.colourPropertyName), "Colour default value");
		Assertions.assertEquals(new DateOnly("2021-10-21"), Binder.get(bean, AllAttributesPersistent.datePropertyName), "Date default value");
		Assertions.assertEquals(new DateTime("2021-10-21T07:48:29Z"), Binder.get(bean, AllAttributesPersistent.dateTimePropertyName), "DateTime default value");
		Assertions.assertEquals(new Decimal10("100.1234567899"), Binder.get(bean, AllAttributesPersistent.decimal10PropertyName), "Decimal10 default value");
		Assertions.assertEquals(new Decimal2("100.12"), Binder.get(bean, AllAttributesPersistent.decimal2PropertyName), "Decimal2 default value");
		Assertions.assertEquals(new Decimal5("100.12345"), Binder.get(bean, AllAttributesPersistent.decimal5PropertyName), "Decimal5 default value");
		Assertions.assertEquals("one", Binder.get(bean, AllAttributesPersistent.enum3PropertyName), "Enum default value");
		Geometry g = (Geometry) Binder.fromSerialised(Geometry.class, "POINT(0 0)");
		Assertions.assertEquals(g, Binder.get(bean, AllAttributesPersistent.geometryPropertyName), "Geometry default value");
		Assertions.assertEquals("1234567890", Binder.get(bean, AllAttributesPersistent.idPropertyName), "Id default value");
		Assertions.assertEquals(Integer.valueOf(123), Binder.get(bean, AllAttributesPersistent.normalIntegerPropertyName), "Integer default value");
		Assertions.assertEquals(Long.valueOf(123), Binder.get(bean, AllAttributesPersistent.longIntegerPropertyName), "Long integer default value");
		Assertions.assertEquals("<h1>Markup</h1>", Binder.get(bean, AllAttributesPersistent.markupPropertyName), "Markup default value");
		Assertions.assertEquals("Memo", Binder.get(bean, AllAttributesPersistent.memoPropertyName), "Memo default value");
		Assertions.assertEquals("Text", Binder.get(bean, AllAttributesPersistent.textPropertyName), "Text default value");
		Assertions.assertEquals(new TimeOnly("07:51:26"), Binder.get(bean, AllAttributesPersistent.timePropertyName), "Time default value");
		Assertions.assertEquals(new Timestamp("2021-10-21T07:48:29Z"), Binder.get(bean, AllAttributesPersistent.timestampPropertyName), "Timestamp default value");
		
		bean = adapd.newInstance(u);
		Assertions.assertEquals("#000000", Binder.get(bean, AllAttributesPersistent.colourPropertyName), "Colour default value");
	}
	
	@Test
	void testJSON() throws Exception {
		Bean bean = Util.constructRandomInstance(u, m, aadpd, 2);
		String bizId = bean.getBizId();
		String json = JSON.marshall(c, bean);
		bean = (Bean) JSON.unmarshall(u, json);
		Assertions.assertEquals(bizId, bean.getBizId(), "JSON marshall/unmarshall problem");
		Assertions.assertEquals(DynamicPersistentBean.class, bean.getClass(), "JSON unmarshall document should create AllAtrributesDynamicPersistent");

		bean = Util.constructRandomInstance(u, m, adapd, 2);
		bizId = bean.getBizId();
		json = JSON.marshall(c, bean);
		bean = (Bean) JSON.unmarshall(u, json);
		Assertions.assertEquals(bizId, bean.getBizId(), "JSON marshall/unmarshall problem");
		Assertions.assertEquals(AllDynamicAttributesPersistent.class, bean.getClass(), "JSON unmarshall document should create AllAtrributesPersistent");
	}
	
	@Test
	void testBinderWithDynamicAttributes() throws Exception {
		Bean bean = Util.constructRandomInstance(u, m, adapd, 2);
		AllDynamicAttributesPersistent testAssignment = AllDynamicAttributesPersistent.newInstance();
		testBinder(bean, AllDynamicAttributesPersistent.aggregatedAssociationPropertyName, AllDynamicAttributesPersistent.composedCollectionPropertyName, testAssignment);
	}

	@Test
	void testBinderWithDynamicDocument() throws Exception {
		Bean bean = Util.constructRandomInstance(u, m, aadpd, 2);
		AllAttributesPersistent testAssignment = AllAttributesPersistent.newInstance();
		testBinder(bean, AllAttributesPersistent.aggregatedAssociationPropertyName, AllAttributesPersistent.aggregatedCollectionPropertyName, testAssignment);
	}

	private static final Integer INTEGER = Integer.valueOf(Integer.MAX_VALUE);
	
	private static void testBinder(Bean bean, String associationPropertyName, String collectionPropertyName, PersistentBean testAssignment) {
		// Simple scalar
		Binder.set(bean, AllAttributesPersistent.booleanFlagPropertyName, Boolean.TRUE);
		Assertions.assertEquals(Boolean.TRUE, Binder.get(bean, AllAttributesPersistent.booleanFlagPropertyName));
		Binder.set(bean, AllAttributesPersistent.normalIntegerPropertyName, INTEGER);
		Assertions.assertEquals(INTEGER, Binder.get(bean, AllAttributesPersistent.normalIntegerPropertyName));

		// Simple association
		Binder.set(bean, associationPropertyName, bean);
		Assertions.assertEquals(bean, Binder.get(bean, associationPropertyName));

		// Simple collection
		Assertions.assertTrue(Binder.get(bean, collectionPropertyName) instanceof List<?>);
		
		// Compound association to scalar
		String binding = Binder.createCompoundBinding(associationPropertyName, AllAttributesPersistent.normalIntegerPropertyName);
		Binder.set(bean, binding, INTEGER);
		Assertions.assertEquals(INTEGER, Binder.get(bean, binding));
		
		// Compound association to association
		binding = Binder.createCompoundBinding(associationPropertyName, associationPropertyName);
		Object oldValue = Binder.get(bean, binding);
		Binder.set(bean, binding, bean);
		Assertions.assertEquals(bean, Binder.get(bean, binding));
		Binder.set(bean, binding, oldValue);

		// Compound association to collection
		binding = Binder.createCompoundBinding(associationPropertyName, collectionPropertyName);
		Assertions.assertTrue(Binder.get(bean, binding) instanceof List<?>);

		// Compound association to indexed
		binding = Binder.createIndexedBinding(Binder.createCompoundBinding(associationPropertyName, collectionPropertyName), 0);
		Binder.set(bean, binding, testAssignment);
		Assertions.assertEquals(testAssignment, Binder.get(bean, binding));

		// Check set above via Compound association to Id
		binding = Binder.createIdBinding(Binder.createCompoundBinding(associationPropertyName, collectionPropertyName), testAssignment.getBizId());
		Assertions.assertEquals(testAssignment, Binder.get(bean, binding));
		
		// Indexed collection to scalar
		binding = Binder.createCompoundBinding(Binder.createIndexedBinding(collectionPropertyName, 0), AllAttributesPersistent.normalIntegerPropertyName);
		Binder.set(bean, binding, INTEGER);
		Assertions.assertEquals(INTEGER, Binder.get(bean, binding));

		// Id collection to scalar
		@SuppressWarnings("unchecked")
		List<? extends Bean> elements = (List<? extends Bean>) Binder.get(bean, collectionPropertyName);
		elements = Objects.requireNonNull(elements);
		// its a test - let in NPE
		String bizId = elements.get(1).getBizId();
		binding = Binder.createCompoundBinding(Binder.createIdBinding(collectionPropertyName, bizId), AllAttributesPersistent.normalIntegerPropertyName);
		Binder.set(bean, binding, INTEGER);
		Assertions.assertEquals(INTEGER, Binder.get(bean, binding));
	}
	
	@Test
	void testBeanVisitorWithDynamicAttributes() throws Exception {
		Bean bean = Util.constructRandomInstance(u, m, adapd, 2);
		testBeanVisitor(adapd, bean);
	}

	@Test
	void testBeanVisitorWithDynamicDocument() throws Exception {
		Bean bean = Util.constructRandomInstance(u, m, aadpd, 2);
		testBeanVisitor(aadpd, bean);
	}

	private void testBeanVisitor(Document document, Bean bean) {
		new BeanVisitor(false, false) {
			@Override
			protected boolean accept(String binding, Document visitedDocument, Document owningDocument, Relation owningRelation, Bean visitedBean) throws Exception {
				return true;
			}
		}.visit(document, bean, c);

		new BeanVisitor(true, false) {
			@Override
			protected boolean accept(String binding, Document visitedDocument, Document owningDocument, Relation owningRelation, Bean visitedBean) throws Exception {
				return true;
			}
		}.visit(document, bean, c);
	}
	
	@Test
	void testDynamicDefinitions() {
		Bizlet<Bean> bizlet = aadpd.getBizlet(c);
		Assertions.assertNotNull(bizlet);
		
		Object dataFactory = CORE.getRepository().getDataFactory(c, aadpd);
		Assertions.assertNotNull(dataFactory);
		
		ServerSideAction<Bean> action = aadpd.getServerSideAction(c, TestServerSideAction.class.getSimpleName(), true);
		Assertions.assertNotNull(action);

		DynamicImage<Bean> image = aadpd.getDynamicImage(c, TestDynamicImage.class.getSimpleName());
		Assertions.assertNotNull(image);

		ListModel<Bean> model = aadpd.getListModel(c, TestListModel.class.getSimpleName(), false);
		Assertions.assertNotNull(model);
	}
	
	@Test
	void testDynamicInverses() throws Exception {
		// added to inverse collection assigns to other side (dynamic/static)
		DynamicPersistentBean dynamicBean = aadpd.newInstance(u);
		AllDynamicAttributesPersistent staticBean = adapd.newInstance(u);
		BindUtil.addElementToCollection(dynamicBean, AllDynamicAttributesPersistent.dynamicInverseAggregatedAssociationPropertyName, staticBean);
		Assertions.assertSame(dynamicBean, staticBean.getDynamic(AllDynamicAttributesPersistent.dynamicAggregatedAssociationPropertyName));

		// removing from inverse collection nulls other side (dynamic/static)
		BindUtil.removeElementFromCollection(dynamicBean, AllDynamicAttributesPersistent.dynamicInverseAggregatedAssociationPropertyName, staticBean);
		Assertions.assertNull(staticBean.getDynamic(AllDynamicAttributesPersistent.dynamicAggregatedAssociationPropertyName));

		// added to inverse collection assigns to other side (static/dynamic)
		staticBean = adapd.newInstance(u);
		dynamicBean = aadpd.newInstance(u);
		BindUtil.addElementToCollection(staticBean, AllDynamicAttributesPersistent.dynamicInverseAggregatedAssociationPropertyName, dynamicBean);
		Assertions.assertSame(staticBean, dynamicBean.getDynamic(AllDynamicAttributesPersistent.dynamicAggregatedAssociationPropertyName));

		// removing from inverse collection nulls other side (static/dynamic)
		BindUtil.removeElementFromCollection(staticBean, AllDynamicAttributesPersistent.dynamicInverseAggregatedAssociationPropertyName, dynamicBean);
		Assertions.assertNull(dynamicBean.getDynamic(AllDynamicAttributesPersistent.dynamicAggregatedAssociationPropertyName));

		// setting dynamic association adds to inverse collection
		dynamicBean = aadpd.newInstance(u);
		DynamicPersistentBean nutherDynamicBean = aadpd.newInstance(u);
		BindUtil.setAssociation(dynamicBean, AllDynamicAttributesPersistent.dynamicAggregatedAssociationPropertyName, nutherDynamicBean);
		@SuppressWarnings("unchecked")
		List<Bean> list = (List<Bean>) nutherDynamicBean.getDynamic(AllDynamicAttributesPersistent.dynamicInverseAggregatedAssociationPropertyName);
		Assertions.assertTrue(list.contains(dynamicBean));
		
		// overwriting dynamic association removes from old inverse collection and adds to new inverse collection
		DynamicPersistentBean nutherNutherDynamicBean = aadpd.newInstance(u);
		BindUtil.setAssociation(dynamicBean, AllDynamicAttributesPersistent.dynamicAggregatedAssociationPropertyName, nutherNutherDynamicBean);
		//old
		Assertions.assertFalse(list.contains(dynamicBean));
		// new
		@SuppressWarnings("unchecked")
		List<Bean> newList = (List<Bean>) nutherNutherDynamicBean.getDynamic(AllDynamicAttributesPersistent.dynamicInverseAggregatedAssociationPropertyName);
		Assertions.assertTrue(newList.contains(dynamicBean));
	}
}
