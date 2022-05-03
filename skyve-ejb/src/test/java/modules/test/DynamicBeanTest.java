package modules.test;

import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.junit.Assert;
import org.junit.Test;
import org.locationtech.jts.geom.Geometry;
import org.skyve.domain.Bean;
import org.skyve.domain.DynamicBean;
import org.skyve.domain.PersistentDynamicBean;
import org.skyve.domain.types.DateOnly;
import org.skyve.domain.types.DateTime;
import org.skyve.domain.types.Decimal10;
import org.skyve.domain.types.Decimal2;
import org.skyve.domain.types.Decimal5;
import org.skyve.domain.types.TimeOnly;
import org.skyve.domain.types.Timestamp;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.Relation;
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
		contact.setDynamic(ONE, TWO);
		Assert.assertEquals("Dynamic Property set correctly", TWO, contact.getDynamic(ONE));
		Assert.assertTrue("Is a dynamic property", contact.isDynamic(ONE));
		contact.setDynamic(ONE, THREE);
		Assert.assertEquals("Dynamic Property set correctly", THREE, contact.getDynamic(ONE));
		Assert.assertNull("Not a property", contact.getDynamic(THREE));
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
		Assert.assertEquals("AADPD document should create AllAtrributesDynamicPersistent", PersistentDynamicBean.class, aadpd.newInstance(u).getClass());
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
	}
	
	@Test
	public void testJSON() throws Exception {
		Bean bean = Util.constructRandomInstance(u, m, aadpd, 2);
		String bizId = bean.getBizId();
		String json = JSON.marshall(c, bean);
		bean = (Bean) JSON.unmarshall(u, json);
		Assert.assertEquals("JSON marshall/unmarshall problem", bizId, bean.getBizId());
		Assert.assertEquals("JSON unmarshall document should create AllAtrributesDynamicPersistent", PersistentDynamicBean.class, bean.getClass());

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
		testBinder(bean, AllAttributesPersistent.aggregatedAssociationPropertyName, AllAttributesPersistent.aggregatedCollectionPropertyName);
	}

	@Test
	public void testBinderWithDynamicDocument() throws Exception {
		Bean bean = Util.constructRandomInstance(u, m, aadpd, 2);
		testBinder(bean, AllAttributesPersistent.aggregatedAssociationPropertyName, AllAttributesPersistent.aggregatedCollectionPropertyName);
	}

	@Test
	public void testBinderWithDynamicAttributesAndReferences() throws Exception {
		Bean bean = Util.constructRandomInstance(u, m, adapd, 2);
		testBinder(bean, "dynamicAggregatedAssociation", AllAttributesPersistent.aggregatedCollectionPropertyName);
	}

	@Test
	public void testBinderWithDynamicDocumentAndReferences() throws Exception {
		Bean bean = Util.constructRandomInstance(u, m, aadpd, 2);
		testBinder(bean, "dynamicAggregatedAssociation", "dynamicAggregatedCollection");
	}

	private static final Integer INTEGER = Integer.valueOf(Integer.MAX_VALUE);
	
	private static void testBinder(Bean bean, String associationPropertyName, String collectionPropertyName) {
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
		AllAttributesPersistent test = AllAttributesPersistent.newInstance();
		binding = Binder.createIndexedBinding(Binder.createCompoundBinding(associationPropertyName, collectionPropertyName), 0);
		Binder.set(bean, binding, test);
		Assert.assertEquals(test, Binder.get(bean, binding));

		// Check set above via Compound association to Id
		binding = Binder.createIdBinding(Binder.createCompoundBinding(associationPropertyName, collectionPropertyName), test.getBizId());
		Assert.assertEquals(test, Binder.get(bean, binding));
		
		// Indexed collection to scalar
		binding = Binder.createCompoundBinding(Binder.createIndexedBinding(collectionPropertyName, 0), AllAttributesPersistent.normalIntegerPropertyName);
		Binder.set(bean, binding, INTEGER);
		Assert.assertEquals(INTEGER, Binder.get(bean, binding));

		// Id collection to scalar
		@SuppressWarnings("unchecked")
		List<? extends Bean> elements = (List<? extends Bean>) Binder.get(bean, collectionPropertyName);
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
}
