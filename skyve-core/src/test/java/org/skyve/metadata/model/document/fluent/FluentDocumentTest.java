package org.skyve.metadata.model.document.fluent;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.model.document.AssociationImpl;
import org.skyve.impl.metadata.model.document.CollectionImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.model.document.InverseMany;
import org.skyve.impl.metadata.model.document.InverseOne;
import org.skyve.impl.metadata.model.document.field.Colour;
import org.skyve.impl.metadata.model.document.field.Content;
import org.skyve.impl.metadata.model.document.field.Date;
import org.skyve.impl.metadata.model.document.field.DateTime;
import org.skyve.impl.metadata.model.document.field.Decimal10;
import org.skyve.impl.metadata.model.document.field.Decimal2;
import org.skyve.impl.metadata.model.document.field.Decimal5;
import org.skyve.impl.metadata.model.document.field.Enumeration;
import org.skyve.impl.metadata.model.document.field.Geometry;
import org.skyve.impl.metadata.model.document.field.Id;
import org.skyve.impl.metadata.model.document.field.Image;
import org.skyve.impl.metadata.model.document.field.Integer;
import org.skyve.impl.metadata.model.document.field.LongInteger;
import org.skyve.impl.metadata.model.document.field.Markup;
import org.skyve.impl.metadata.model.document.field.Memo;
import org.skyve.impl.metadata.model.document.field.Text;
import org.skyve.impl.metadata.model.document.field.Time;
import org.skyve.impl.metadata.model.document.field.Timestamp;
import org.skyve.metadata.model.Attribute.Sensitivity;
import org.skyve.metadata.model.Dynamic;
import org.skyve.metadata.model.Extends;
import org.skyve.metadata.model.Persistent;
import org.skyve.impl.metadata.model.InterfaceImpl;
import org.skyve.impl.metadata.model.document.ConditionImpl;
import org.skyve.impl.metadata.model.document.UniqueConstraintImpl;

/**
 * Tests for {@link FluentDocument} builder methods.
 */
@SuppressWarnings("static-method")
class FluentDocumentTest {

	// ---- constructors --------------------------------------------------------

	@Test
	void defaultConstructorCreatesInstance() {
		assertThat(new FluentDocument().get(), is(notNullValue()));
	}

	@Test
	void wrappingConstructorPreservesInstance() {
		org.skyve.impl.metadata.repository.document.DocumentMetaData doc =
				new org.skyve.impl.metadata.repository.document.DocumentMetaData();
		assertThat(new FluentDocument(doc).get(), is(doc));
	}

	// ---- scalar string setters -----------------------------------------------

	@Test
	void nameSetsValue() {
		assertEquals("MyDoc", new FluentDocument().name("MyDoc").get().getName());
	}

	@Test
	void singularAliasSetsValue() {
		assertEquals("Item", new FluentDocument().singularAlias("Item").get().getSingularAlias());
	}

	@Test
	void pluralAliasSetsValue() {
		assertEquals("Items", new FluentDocument().pluralAlias("Items").get().getPluralAlias());
	}

	@Test
	void descriptionSetsValue() {
		assertEquals("Desc", new FluentDocument().description("Desc").get().getDescription());
	}

	@Test
	void documentationSetsValue() {
		assertEquals("docs", new FluentDocument().documentation("docs").get().getDocumentation());
	}

	@Test
	void iconStyleClassSetsValue() {
		assertEquals("fa fa-file", new FluentDocument().iconStyleClass("fa fa-file").get().getIconStyleClass());
	}

	@Test
	void icon16x16RelativeFilePathSetsValue() {
		assertEquals("img/icon16.png", new FluentDocument().icon16x16RelativeFilePath("img/icon16.png").get().getIcon16x16RelativeFilePath());
	}

	@Test
	void icon32x32RelativeFilePathSetsValue() {
		assertEquals("img/icon32.png", new FluentDocument().icon32x32RelativeFilePath("img/icon32.png").get().getIcon32x32RelativeFilePath());
	}

	// ---- boolean setters -----------------------------------------------------

	@Test
	void abstractDocumentTrueSetsTrue() {
		assertEquals(Boolean.TRUE, new FluentDocument().abstractDocument(true).get().getAbstract());
	}

	@Test
	void abstractDocumentFalseSetsFalse() {
		assertNotEquals(Boolean.TRUE, new FluentDocument().abstractDocument(false).get().getAbstract());
	}

	@Test
	void auditedTrueSetsTrue() {
		assertEquals(Boolean.TRUE, new FluentDocument().audited(true).get().getAudited());
	}

	@Test
	void auditedFalseSetsFalse() {
		assertNotEquals(Boolean.TRUE, new FluentDocument().audited(false).get().getAudited());
	}

	// ---- bizKey setters ------------------------------------------------------

	@Test
	void bizKeyExpressionSetsExpression() {
		FluentDocument fd = new FluentDocument().bizKeyExpression("{name}");
		assertNotNull(fd.get().getBizKey());
		assertEquals("{name}", fd.get().getBizKey().getExpression());
	}

	@Test
	void bizKeyExpressionCalledTwiceReusesBizKey() {
		FluentDocument fd = new FluentDocument().bizKeyExpression("first").bizKeyExpression("second");
		assertEquals("second", fd.get().getBizKey().getExpression());
	}

	@Test
	void bizKeySensitivitySetsSensitivity() {
		FluentDocument fd = new FluentDocument().bizKeySensitivity(Sensitivity.personal);
		assertNotNull(fd.get().getBizKey());
		assertEquals(Sensitivity.personal, fd.get().getBizKey().getSensitivity());
	}

	// ---- extendsDocument ----------------------------------------------------

	@Test
	void extendsDocumentSetsBaseDocumentName() {
		FluentDocument fd = new FluentDocument().extendsDocument("BaseDoc");
		assertNotNull(fd.get().getExtends());
		assertEquals("BaseDoc", fd.get().getExtends().getDocumentName());
	}

	// ---- parentDocument -----------------------------------------------------

	@Test
	void parentDocumentSetsParentDocumentMetaData() {
		FluentDocument fd = new FluentDocument().parentDocument(new FluentParentDocument().parentDocumentName("ParentDoc"));
		assertNotNull(fd.get().getParentDocument());
	}

	// ---- persistent ---------------------------------------------------------

	@Test
	void persistentSetsPersistentMetaData() {
		FluentDocument fd = new FluentDocument().persistent(new FluentPersistent().name("TBL_DOC"));
		assertNotNull(fd.get().getPersistent());
	}

	// ---- dynamic ------------------------------------------------------------

	@Test
	void dynamicSetsDynamicMetaData() {
		FluentDocument fd = new FluentDocument().dynamic(new FluentDynamic());
		assertNotNull(fd.get().getDynamic());
	}

	// ---- implementing interfaces --------------------------------------------

	@Test
	void addImplementingInterfaceAddsInterface() {
		FluentDocument fd = new FluentDocument().addImplementingInterface("com.example.IFoo");
		assertFalse(fd.get().getImplements().isEmpty());
	}

	@Test
	void removeImplementingInterfaceRemovesIt() {
		FluentDocument fd = new FluentDocument()
				.addImplementingInterface("com.example.IFoo")
				.removeImplementingInterface("com.example.IFoo");
		assertTrue(fd.get().getImplements().isEmpty());
	}

	// ---- text attribute -----------------------------------------------------

	@Test
	void addTextAndFindTextByName() {
		FluentText text = new FluentText().name("description");
		FluentDocument fd = new FluentDocument().addText(text);
		assertNotNull(fd.findText("description"));
	}

	@Test
	void findTextReturnsNullWhenAbsent() {
		assertNull(new FluentDocument().findText("missing"));
	}

	// ---- date attribute -----------------------------------------------------

	@Test
	void addDateAndFindDateByName() {
		FluentDate date = new FluentDate().name("startDate");
		FluentDocument fd = new FluentDocument().addDate(date);
		assertNotNull(fd.findDate("startDate"));
	}

	@Test
	void findDateReturnsNullWhenAbsent() {
		assertNull(new FluentDocument().findDate("missing"));
	}

	// ---- time attribute -----------------------------------------------------

	@Test
	void addTimeAndFindTimeByName() {
		FluentTime time = new FluentTime().name("startTime");
		FluentDocument fd = new FluentDocument().addTime(time);
		assertNotNull(fd.findTime("startTime"));
	}

	@Test
	void findTimeReturnsNullWhenAbsent() {
		assertNull(new FluentDocument().findTime("missing"));
	}

	// ---- dateTime attribute -------------------------------------------------

	@Test
	void addDateTimeAndFindDateTimeByName() {
		FluentDateTime dt = new FluentDateTime().name("createdAt");
		FluentDocument fd = new FluentDocument().addDateTime(dt);
		assertNotNull(fd.findDateTime("createdAt"));
	}

	@Test
	void findDateTimeReturnsNullWhenAbsent() {
		assertNull(new FluentDocument().findDateTime("missing"));
	}

	// ---- timestamp attribute ------------------------------------------------

	@Test
	void addTimestampAndFindTimestampByName() {
		FluentTimestamp ts = new FluentTimestamp().name("modifiedAt");
		FluentDocument fd = new FluentDocument().addTimestamp(ts);
		assertNotNull(fd.findTimestamp("modifiedAt"));
	}

	@Test
	void findTimestampReturnsNullWhenAbsent() {
		assertNull(new FluentDocument().findTimestamp("missing"));
	}

	// ---- integer attribute --------------------------------------------------

	@Test
	void addIntegerAndFindIntegerByName() {
		FluentInteger fi = new FluentInteger().name("count");
		FluentDocument fd = new FluentDocument().addInteger(fi);
		assertNotNull(fd.findInteger("count"));
	}

	@Test
	void findIntegerReturnsNullWhenAbsent() {
		assertNull(new FluentDocument().findInteger("missing"));
	}

	// ---- longInteger attribute ----------------------------------------------

	@Test
	void addLongIntegerAndFindLongIntegerByName() {
		FluentLongInteger fli = new FluentLongInteger().name("bigCount");
		FluentDocument fd = new FluentDocument().addLongInteger(fli);
		assertNotNull(fd.findLongInteger("bigCount"));
	}

	@Test
	void findLongIntegerReturnsNullWhenAbsent() {
		assertNull(new FluentDocument().findLongInteger("missing"));
	}

	// ---- decimal2 attribute -------------------------------------------------

	@Test
	void addDecimal2AndFindDecimal2ByName() {
		FluentDecimal2 fd2 = new FluentDecimal2().name("price");
		FluentDocument fd = new FluentDocument().addDecimal2(fd2);
		assertNotNull(fd.findDecimal2("price"));
	}

	@Test
	void findDecimal2ReturnsNullWhenAbsent() {
		assertNull(new FluentDocument().findDecimal2("missing"));
	}

	// ---- decimal5 attribute -------------------------------------------------

	@Test
	void addDecimal5AndFindDecimal5ByName() {
		FluentDecimal5 fd5 = new FluentDecimal5().name("rate");
		FluentDocument fd = new FluentDocument().addDecimal5(fd5);
		assertNotNull(fd.findDecimal5("rate"));
	}

	@Test
	void findDecimal5ReturnsNullWhenAbsent() {
		assertNull(new FluentDocument().findDecimal5("missing"));
	}

	// ---- decimal10 attribute ------------------------------------------------

	@Test
	void addDecimal10AndFindDecimal10ByName() {
		FluentDecimal10 fd10 = new FluentDecimal10().name("precision");
		FluentDocument fd = new FluentDocument().addDecimal10(fd10);
		assertNotNull(fd.findDecimal10("precision"));
	}

	@Test
	void findDecimal10ReturnsNullWhenAbsent() {
		assertNull(new FluentDocument().findDecimal10("missing"));
	}

	// ---- boolean attribute --------------------------------------------------

	@Test
	void addBooleanAndFindBooleanByName() {
		FluentBoolean fb = new FluentBoolean().name("active");
		FluentDocument fd = new FluentDocument().addBoolean(fb);
		assertNotNull(fd.findBoolean("active"));
	}

	@Test
	void findBooleanReturnsNullWhenAbsent() {
		assertNull(new FluentDocument().findBoolean("missing"));
	}

	// ---- enumeration attribute ----------------------------------------------

	@Test
	void addEnumerationAndFindEnumerationByName() {
		FluentEnumeration fe = new FluentEnumeration().name("status");
		FluentDocument fd = new FluentDocument().addEnumeration(fe);
		assertNotNull(fd.findEnumeration("status"));
	}

	@Test
	void findEnumerationReturnsNullWhenAbsent() {
		assertNull(new FluentDocument().findEnumeration("missing"));
	}

	// ---- memo attribute -----------------------------------------------------

	@Test
	void addMemoAndFindMemoByName() {
		FluentMemo fm = new FluentMemo().name("notes");
		FluentDocument fd = new FluentDocument().addMemo(fm);
		assertNotNull(fd.findMemo("notes"));
	}

	@Test
	void findMemoReturnsNullWhenAbsent() {
		assertNull(new FluentDocument().findMemo("missing"));
	}

	// ---- markup attribute ---------------------------------------------------

	@Test
	void addMarkupAndFindMarkupByName() {
		FluentMarkup fmu = new FluentMarkup().name("body");
		FluentDocument fd = new FluentDocument().addMarkup(fmu);
		assertNotNull(fd.findMarkup("body"));
	}

	@Test
	void findMarkupReturnsNullWhenAbsent() {
		assertNull(new FluentDocument().findMarkup("missing"));
	}

	// ---- colour attribute ---------------------------------------------------

	@Test
	void addColourAndFindColourByName() {
		FluentColour fc = new FluentColour().name("bg");
		FluentDocument fd = new FluentDocument().addColour(fc);
		assertNotNull(fd.findColour("bg"));
	}

	@Test
	void findColourReturnsNullWhenAbsent() {
		assertNull(new FluentDocument().findColour("missing"));
	}

	// ---- content attribute --------------------------------------------------

	@Test
	void addContentAndFindContentByName() {
		FluentContent fco = new FluentContent().name("attachment");
		FluentDocument fd = new FluentDocument().addContent(fco);
		assertNotNull(fd.findContent("attachment"));
	}

	@Test
	void findContentReturnsNullWhenAbsent() {
		assertNull(new FluentDocument().findContent("missing"));
	}

	// ---- image attribute ----------------------------------------------------

	@Test
	void addImageAndFindImageByName() {
		FluentImage fi = new FluentImage().name("photo");
		FluentDocument fd = new FluentDocument().addImage(fi);
		assertNotNull(fd.findImage("photo"));
	}

	@Test
	void findImageReturnsNullWhenAbsent() {
		assertNull(new FluentDocument().findImage("missing"));
	}

	// ---- geometry attribute -------------------------------------------------

	@Test
	void addGeometryAndFindGeometryByName() {
		FluentGeometry fg = new FluentGeometry().name("location");
		FluentDocument fd = new FluentDocument().addGeometry(fg);
		assertNotNull(fd.findGeometry("location"));
	}

	@Test
	void findGeometryReturnsNullWhenAbsent() {
		assertNull(new FluentDocument().findGeometry("missing"));
	}

	// ---- id attribute -------------------------------------------------------

	@Test
	void addIdAndFindIdByName() {
		FluentId fid = new FluentId().name("externalId");
		FluentDocument fd = new FluentDocument().addId(fid);
		assertNotNull(fd.findId("externalId"));
	}

	@Test
	void findIdReturnsNullWhenAbsent() {
		assertNull(new FluentDocument().findId("missing"));
	}

	// ---- association attribute ----------------------------------------------

	@Test
	void addAssociationAndFindAssociationByName() {
		FluentAssociation fa = new FluentAssociation().name("owner");
		FluentDocument fd = new FluentDocument().addAssociation(fa);
		assertNotNull(fd.findAssociation("owner"));
	}

	@Test
	void findAssociationReturnsNullWhenAbsent() {
		assertNull(new FluentDocument().findAssociation("missing"));
	}

	// ---- collection attribute -----------------------------------------------

	@Test
	void addCollectionAndFindCollectionByName() {
		FluentCollection fco = new FluentCollection().name("items");
		FluentDocument fd = new FluentDocument().addCollection(fco);
		assertNotNull(fd.findCollection("items"));
	}

	@Test
	void findCollectionReturnsNullWhenAbsent() {
		assertNull(new FluentDocument().findCollection("missing"));
	}

	// ---- inverseOne attribute -----------------------------------------------

	@Test
	void addInverseOneAndFindInverseOneByName() {
		FluentInverseOne fio = new FluentInverseOne().name("parent");
		FluentDocument fd = new FluentDocument().addInverseOne(fio);
		assertNotNull(fd.findInverseOne("parent"));
	}

	@Test
	void findInverseOneReturnsNullWhenAbsent() {
		assertNull(new FluentDocument().findInverseOne("missing"));
	}

	// ---- inverseMany attribute ----------------------------------------------

	@Test
	void addInverseManyAndFindInverseManyByName() {
		FluentInverseMany fim = new FluentInverseMany().name("children");
		FluentDocument fd = new FluentDocument().addInverseMany(fim);
		assertNotNull(fd.findInverseMany("children"));
	}

	@Test
	void findInverseManyReturnsNullWhenAbsent() {
		assertNull(new FluentDocument().findInverseMany("missing"));
	}

	// ---- removeAttribute / clearAttributes ----------------------------------

	@Test
	void removeAttributeRemovesItFromList() {
		FluentDocument fd = new FluentDocument()
				.addText(new FluentText().name("title"));
		fd.removeAttribute("title");
		assertNull(fd.findText("title"));
	}

	@Test
	void clearAttributesEmptiesAll() {
		FluentDocument fd = new FluentDocument()
				.addText(new FluentText().name("a"))
				.addInteger(new FluentInteger().name("b"));
		fd.clearAttributes();
		assertTrue(fd.get().getAttributes().isEmpty());
	}

	// ---- conditions ---------------------------------------------------------

	@Test
	void addConditionAndFindConditionByName() {
		FluentCondition cond = new FluentCondition().name("isActive");
		FluentDocument fd = new FluentDocument().addCondition(cond);
		assertNotNull(fd.findCondition("isActive"));
	}

	@Test
	void findConditionReturnsNullWhenAbsent() {
		assertNull(new FluentDocument().findCondition("missing"));
	}

	@Test
	void removeConditionRemovesIt() {
		FluentDocument fd = new FluentDocument()
				.addCondition(new FluentCondition().name("isActive"));
		fd.removeCondition("isActive");
		assertNull(fd.findCondition("isActive"));
	}

	@Test
	void clearConditionsEmptiesAll() {
		FluentDocument fd = new FluentDocument()
				.addCondition(new FluentCondition().name("c1"))
				.addCondition(new FluentCondition().name("c2"));
		fd.clearConditions();
		assertTrue(fd.get().getConditions().isEmpty());
	}

	// ---- unique constraints -------------------------------------------------

	@Test
	void addUniqueConstraintAndFindByName() {
		FluentDocumentUniqueConstraint uc = new FluentDocumentUniqueConstraint().name("UC_name");
		FluentDocument fd = new FluentDocument().addUniqueConstraint(uc);
		assertNotNull(fd.findUniqueConstraint("UC_name"));
	}

	@Test
	void findUniqueConstraintReturnsNullWhenAbsent() {
		assertThat(new FluentDocument().findUniqueConstraint("missing"), is(nullValue()));
	}

	@Test
	void removeUniqueConstraintRemovesIt() {
		FluentDocument fd = new FluentDocument()
				.addUniqueConstraint(new FluentDocumentUniqueConstraint().name("UC_name"));
		fd.removeUniqueConstraint("UC_name");
		assertNull(fd.findUniqueConstraint("UC_name"));
	}

	@Test
	void clearUniqueConstraintsEmptiesAll() {
		FluentDocument fd = new FluentDocument()
				.addUniqueConstraint(new FluentDocumentUniqueConstraint().name("UC_1"))
				.addUniqueConstraint(new FluentDocumentUniqueConstraint().name("UC_2"));
		fd.clearUniqueConstraint();
		assertTrue(fd.get().getUniqueConstraints().isEmpty());
	}

	// ---- addAttribute generic ----------------------------------------------

	@Test
	void addAttributeGenericAddsToList() {
		FluentText text = new FluentText().name("generic");
		FluentDocument fd = new FluentDocument().addAttribute(text);
		assertFalse(fd.get().getAttributes().isEmpty());
	}

	// ---- from(Document) --------------------------------------------------

	@Test
	void fromWithEmptyDocumentProducesValidResult() {
		DocumentImpl doc = new DocumentImpl();
		doc.setName("TestDoc");
		FluentDocument fd = new FluentDocument().from(doc);
		assertEquals("TestDoc", fd.get().getName());
	}

	@Test
	void fromWithAllFieldTypesCoversInstanceofChain() {
		DocumentImpl doc = new DocumentImpl();
		doc.setName("AllFields");

		Text text = new Text();
		text.setName("textField");
		doc.putAttribute(text);

		org.skyve.impl.metadata.model.document.field.Boolean bool = new org.skyve.impl.metadata.model.document.field.Boolean();
		bool.setName("boolField");
		doc.putAttribute(bool);

		Enumeration enumeration = new Enumeration();
		enumeration.setName("enumField");
		doc.putAttribute(enumeration);

		Markup markup = new Markup();
		markup.setName("markupField");
		doc.putAttribute(markup);

		Memo memo = new Memo();
		memo.setName("memoField");
		doc.putAttribute(memo);

		Date date = new Date();
		date.setName("dateField");
		doc.putAttribute(date);

		Integer integer = new Integer();
		integer.setName("intField");
		doc.putAttribute(integer);

		AssociationImpl assoc = new AssociationImpl();
		assoc.setName("assocField");
		doc.putAttribute(assoc);

		CollectionImpl collection = new CollectionImpl();
		collection.setName("collectionField");
		doc.putAttribute(collection);

		LongInteger longInt = new LongInteger();
		longInt.setName("longIntField");
		doc.putAttribute(longInt);

		Decimal2 dec2 = new Decimal2();
		dec2.setName("dec2Field");
		doc.putAttribute(dec2);

		Decimal5 dec5 = new Decimal5();
		dec5.setName("dec5Field");
		doc.putAttribute(dec5);

		Decimal10 dec10 = new Decimal10();
		dec10.setName("dec10Field");
		doc.putAttribute(dec10);

		Time time = new Time();
		time.setName("timeField");
		doc.putAttribute(time);

		DateTime dateTime = new DateTime();
		dateTime.setName("dateTimeField");
		doc.putAttribute(dateTime);

		Timestamp timestamp = new Timestamp();
		timestamp.setName("timestampField");
		doc.putAttribute(timestamp);

		Colour colour = new Colour();
		colour.setName("colourField");
		doc.putAttribute(colour);

		Content content = new Content();
		content.setName("contentField");
		doc.putAttribute(content);

		Image image = new Image();
		image.setName("imageField");
		doc.putAttribute(image);

		Geometry geometry = new Geometry();
		geometry.setName("geometryField");
		doc.putAttribute(geometry);

		Id id = new Id();
		id.setName("idField");
		doc.putAttribute(id);

		InverseOne inverseOne = new InverseOne();
		inverseOne.setName("invOneField");
		doc.putAttribute(inverseOne);

		InverseMany inverseMany = new InverseMany();
		inverseMany.setName("invManyField");
		doc.putAttribute(inverseMany);

		FluentDocument fd = new FluentDocument().from(doc);
		// 23 attributes should have been copied
		assertEquals(23, fd.get().getAttributes().size());
	}

	@Test
	void fromWithExtendsAndParentDocumentAndDynamicAndInterfaces() {
		// Covers the extends, parentDocument, persistent, dynamic, and interfaces paths in from()
		DocumentImpl doc = new DocumentImpl();
		doc.setName("FullDoc");

		// Set extends
		Extends inherits = new Extends();
		inherits.setDocumentName("BaseDoc");
		doc.setExtends(inherits);

		// Set parentDocument (child scenario: different name from doc name)
		doc.setParentDocumentName("ParentDoc");
		doc.setParentDatabaseIndex(Boolean.TRUE);

		// Set persistent
		Persistent persistent = new Persistent();
		persistent.setName("FULL_DOC");
		doc.setPersistent(persistent);

		// Set dynamic
		Dynamic dynamic = new Dynamic();
		doc.setDynamism(dynamic);

		// Add an interface
		InterfaceImpl iface = new InterfaceImpl();
		iface.setInterfaceName("java.io.Serializable");
		doc.putInterface(iface);

		FluentDocument fd = new FluentDocument().from(doc);

		assertNotNull(fd.get().getExtends());
		assertEquals("BaseDoc", fd.get().getExtends().getDocumentName());
		assertNotNull(fd.get().getParentDocument());
		assertNotNull(fd.get().getPersistent());
		assertNotNull(fd.get().getDynamic());
		assertFalse(fd.get().getImplements().isEmpty());
	}

	@Test
	void parentDocumentWrappingConstructorPreservesInstance() {
		org.skyve.impl.metadata.repository.document.ParentDocument pd = new org.skyve.impl.metadata.repository.document.ParentDocument();
		assertSame(pd, new FluentParentDocument(pd).get());
	}

	@Test
	void persistentWrappingConstructorPreservesInstance() {
		Persistent p = new Persistent();
		assertSame(p, new FluentPersistent(p).get());
	}

	@Test
	void fromWithConditionCoversConditionLoopBody() {
		DocumentImpl doc = new DocumentImpl();
		ConditionImpl cond = new ConditionImpl();
		cond.setExpression("true");
		doc.getConditions().put("isActive", cond);
		FluentDocument fd = new FluentDocument().from(doc);
		assertNotNull(fd.findCondition("isActive"));
	}

	@Test
	void fromWithUniqueConstraintCoversConstraintLoopBody() {
		DocumentImpl doc = new DocumentImpl();
		UniqueConstraintImpl uc = new UniqueConstraintImpl();
		uc.setName("UC_email");
		doc.putUniqueConstraint(uc);
		FluentDocument fd = new FluentDocument().from(doc);
		assertNotNull(fd.findUniqueConstraint("UC_email"));
	}
}
