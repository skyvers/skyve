package org.skyve.metadata.model.document.fluent;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.model.AbstractAttribute;
import org.skyve.domain.types.converters.Format.TextCase;
import org.skyve.impl.metadata.OrderingImpl;
import org.skyve.impl.metadata.model.document.AssociationImpl;
import org.skyve.impl.metadata.model.document.CollectionImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.model.document.InverseMany;
import org.skyve.impl.metadata.model.document.UniqueConstraintImpl;
import org.skyve.impl.metadata.repository.document.ConditionMetaData;
import org.skyve.impl.metadata.model.document.field.Colour;
import org.skyve.impl.metadata.model.document.field.Content;
import org.skyve.impl.metadata.model.document.field.Enumeration;
import org.skyve.impl.metadata.model.document.field.Enumeration.EnumeratedValue;
import org.skyve.impl.metadata.model.document.field.Field.GeneratedType;
import org.skyve.impl.metadata.model.document.field.Field.IndexType;
import org.skyve.impl.metadata.model.document.field.Id;
import org.skyve.impl.metadata.model.document.field.Image;
import org.skyve.impl.metadata.model.document.field.TextFormat;
import org.skyve.impl.metadata.model.document.field.validator.TextValidator;
import org.skyve.impl.metadata.model.document.field.validator.TextValidator.ValidatorType;
import org.skyve.impl.metadata.repository.document.ParentDocument;
import org.skyve.metadata.ConverterName;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.model.Dynamic;
import org.skyve.metadata.model.Persistent;
import org.skyve.metadata.model.Persistent.ExtensionStrategy;
import org.skyve.metadata.model.document.Association.AssociationType;
import org.skyve.metadata.model.document.Collection.CollectionType;
import org.skyve.metadata.model.document.DomainType;
import org.skyve.metadata.model.document.UniqueConstraint.DocumentScope;

@SuppressWarnings("static-method")
class FluentDocumentAdditionalBuildersTest {

	@Test
	void collectionBuilderSupportsOrderingAndConstraintMutations() {
		FluentCollection collection = new FluentCollection()
				.name("children")
				.type(CollectionType.child)
				.ordered(true)
				.minCardinality(1)
				.maxCardinality(5)
				.ownerDatabaseIndex(true)
				.elementDatabaseIndex(false)
				.cacheName("child-cache");

		FluentCollectionOrdering ordering = new FluentCollectionOrdering().by("code").sort(SortDirection.descending);
		FluentCollectionUniqueConstraint constraint = new FluentCollectionUniqueConstraint()
				.name("uqChildren")
				.scope(DocumentScope.customer)
				.addFieldName("code");

		collection.addOrdering(ordering).addUniqueConstraint(constraint);

		assertThat(collection.findOrdering("code"), is(notNullValue()));
		assertThat(collection.findUniqueConstraint("uqChildren"), is(notNullValue()));
		assertThat(collection.get().getCacheName(), is("child-cache"));
		assertThat(collection.get().getOrdered(), is(Boolean.TRUE));

		collection.removeOrdering("code").removeUniqueConstraint("uqChildren");
		assertTrue(collection.get().getOrdering().isEmpty());
		assertTrue(collection.get().getUniqueConstraints().isEmpty());
	}

	@Test
	void collectionFromCopiesNestedOrderingsAndConstraints() {
		CollectionImpl source = new CollectionImpl();
		source.setName("orders");
		source.setType(CollectionType.child);
		source.setOrdered(Boolean.TRUE);
		source.setMinCardinality(2);
		source.setMaxCardinality(Integer.valueOf(7));
		source.setOwnerDatabaseIndex(Boolean.TRUE);
		source.setElementDatabaseIndex(Boolean.TRUE);
		source.setCacheName("orders-cache");

		OrderingImpl ordering = new OrderingImpl();
		ordering.setBy("created");
		ordering.setSort(SortDirection.ascending);
		source.getOrdering().add(ordering);

		UniqueConstraintImpl constraint = new UniqueConstraintImpl();
		constraint.setName("uqOrders");
		constraint.setScope(DocumentScope.customer);
		constraint.getFieldNames().add("number");
		source.getUniqueConstraints().add(constraint);

		FluentCollection fluent = new FluentCollection().from(source);

		assertThat(fluent.get().getName(), is("orders"));
		assertThat(fluent.get().getMaxCardinality(), is(Integer.valueOf(7)));
		assertThat(fluent.findOrdering("created"), is(notNullValue()));
		assertThat(fluent.findUniqueConstraint("uqOrders"), is(notNullValue()));
	}

	@Test
	void associationAndInverseBuildersCopyReferenceFields() {
		AssociationImpl association = new AssociationImpl();
		association.setName("customer");
		association.setPersistent(true);
		association.setDomainType(DomainType.dynamic);
		association.setDocumentName("Customer");
		association.setQueryName("qCustomer");
		association.setRequired(true);
		association.setRequiredMessage("required");
		association.setType(AssociationType.aggregation);
		association.setEmbeddedColumnsPrefix("cust_");
		association.setDatabaseIndex(Boolean.TRUE);

		FluentAssociation fluentAssociation = new FluentAssociation().from(association);
		assertThat(fluentAssociation.get().getDocumentName(), is("Customer"));
		assertThat(fluentAssociation.get().getType(), is(AssociationType.aggregation));
		assertThat(fluentAssociation.get().getDatabaseIndex(), is(Boolean.TRUE));

		InverseMany inverse = new InverseMany();
		inverse.setDocumentName("Invoice");
		inverse.setReferenceName("customer");
		inverse.setCascade(Boolean.TRUE);

		FluentInverseMany fluentInverse = new FluentInverseMany().from(inverse);
		assertThat(fluentInverse.get().getCascade(), is(Boolean.TRUE));
		assertThat(fluentInverse.get().getDocumentName(), is("customer"));
	}

	@Test
	void contentImageAndInverseOneBuildersCopyValues() {
		Content content = new Content();
		content.setName("attachment");
		content.setDomainType(DomainType.dynamic);
		FluentContent fluentContent = new FluentContent().from(content);
		assertThat(fluentContent.get().getName(), is("attachment"));
		assertThat(fluentContent.get().getDomainType(), is(DomainType.dynamic));

		Image image = new Image();
		image.setName("photo");
		image.setDomainType(DomainType.variant);
		FluentImage fluentImage = new FluentImage().from(image);
		assertThat(fluentImage.get().getName(), is("photo"));
		assertThat(fluentImage.get().getDomainType(), is(DomainType.variant));

		org.skyve.impl.metadata.model.document.InverseOne inverse = new org.skyve.impl.metadata.model.document.InverseOne();
		inverse.setDocumentName("Order");
		inverse.setReferenceName("customer");
		inverse.setCascade(Boolean.TRUE);
		FluentInverseOne fluentInverse = new FluentInverseOne().from(inverse);
		assertThat(fluentInverse.get().getCascade(), is(Boolean.TRUE));
		assertThat(fluentInverse.get().getDocumentName(), is("customer"));
	}

	@Test
	void enumerationBuilderSupportsLookupAndMutations() {
		Enumeration source = new Enumeration();
		source.setName("status");
		source.setXmlTypeName("StatusType");
		source.setXmlImplementingEnumClassName("example.Status");
		source.setModuleRef("admin");
		source.setDocumentRef("Order");

		EnumeratedValue sourceValue = new EnumeratedValue();
		sourceValue.setName("Open");
		sourceValue.setCode("O");
		sourceValue.setDescription("Open state");
		source.getXmlValues().add(sourceValue);

		FluentEnumeration fluent = new FluentEnumeration().from(source)
				.addValue(new FluentEnumeratedValue().name("Closed").code("C").description("Closed state"));

		assertThat(fluent.findValueByName("Open"), is(notNullValue()));
		assertThat(fluent.findValueByCode("C"), is(notNullValue()));
		assertThat(fluent.findValueByDescription("Closed state"), is(notNullValue()));

		fluent.removeValueByCode("C").removeValueByName("Open").clearValues();
		assertTrue(fluent.get().getXmlValues().isEmpty());
	}

	@Test
	void dynamicBuilderCopiesAndMutatesMaps() {
		Dynamic source = new Dynamic();
		source.setBizletClassName("modules.admin.OrderBizlet");
		source.setDataFactoryClassName("modules.admin.OrderFactory");
		source.getActions().put("approve", "modules.admin.actions.Approve");
		source.getImages().put("photo", "modules.admin.images.Photo");
		source.getModels().put("summary", "modules.admin.models.Summary");

		FluentDynamic fluent = new FluentDynamic().from(source)
				.addAction("cancel", "modules.admin.actions.Cancel")
				.addImage("thumb", "modules.admin.images.Thumb")
				.addModel("detail", "modules.admin.models.Detail");

		assertTrue(fluent.get().getActions().containsKey("approve"));
		assertTrue(fluent.get().getImages().containsKey("thumb"));
		assertTrue(fluent.get().getModels().containsKey("detail"));

		fluent.removeAction("approve").removeImage("photo").removeModel("summary");
		assertFalse(fluent.get().getActions().containsKey("approve"));
	}

	@Test
	void fieldDerivedBuildersCopyInheritedProperties() {
		Id id = new Id();
		id.setName("identifier");
		id.setRequired(true);
		id.setRequiredMessage("id required");
		id.setPersistent(false);
		id.setDynamic(true);
		id.setIndex(IndexType.database);
		id.setDefaultValue("100");
		id.setGenerated(GeneratedType.insert);

		FluentId fluentId = new FluentId().from(id);
		assertThat(fluentId.get().getRequiredMessage(), is("id required"));
		assertThat(fluentId.get().getIndex(), is(IndexType.database));
		assertThat(fluentId.get().getGenerated(), is(GeneratedType.insert));

		Colour colour = new Colour();
		colour.setName("themeColour");
		colour.setDomainType(DomainType.constant);
		colour.setConverterName(ConverterName.HH_MI);

		FluentColour fluentColour = new FluentColour().from(colour);
		assertThat(fluentColour.get().getDomainType(), is(DomainType.constant));
		assertThat(fluentColour.get().getConverterName(), is(ConverterName.HH_MI));
	}

	@Test
	void textFormatAndValidatorBuildersCopyValues() {
		TextFormat format = new TextFormat();
		format.setMask("AAA-999");
		format.setCase(TextCase.upper);

		FluentTextFormat fluentFormat = new FluentTextFormat().from(format);
		assertThat(fluentFormat.get().getMask(), is("AAA-999"));
		assertThat(fluentFormat.get().getCase(), is(TextCase.upper));

		TextValidator validator = new TextValidator();
		validator.setType(ValidatorType.email);
		validator.setRegularExpression("[A-Z]{3}\\d{3}");
		validator.setValidationMessage("invalid");

		FluentTextValidator fluentValidator = new FluentTextValidator().from(validator);
		assertThat(fluentValidator.get().getType(), is(ValidatorType.email));
		assertThat(fluentValidator.get().getRegularExpression(), is("[A-Z]{3}\\d{3}"));
	}

	@Test
	void persistentParentAndUniqueConstraintBuildersCopyValues() {
		Persistent persistent = new Persistent();
		persistent.setName("T_ORDER");
		persistent.setSchema("PUBLIC");
		persistent.setCatalog("CAT");
		persistent.setStrategy(ExtensionStrategy.single);
		persistent.setDiscriminator("ORD");
		persistent.setCacheName("orders");

		FluentPersistent fluentPersistent = new FluentPersistent().from(persistent);
		assertThat(fluentPersistent.get().getName(), is("T_ORDER"));
		assertThat(fluentPersistent.get().getStrategy(), is(ExtensionStrategy.single));

		ParentDocument parent = new ParentDocument();
		parent.setParentDocumentName("BaseDocument");
		parent.setDatabaseIndex(Boolean.FALSE);
		FluentParentDocument fluentParent = new FluentParentDocument().from(parent);
		assertThat(fluentParent.get().getParentDocumentName(), is("BaseDocument"));
		assertThat(fluentParent.get().getDatabaseIndex(), is(Boolean.FALSE));

		UniqueConstraintImpl unique = new UniqueConstraintImpl();
		unique.setName("uqOrder");
		unique.setDescription("Unique order");
		unique.setScope(DocumentScope.customer);
		unique.setMessage("already exists");
		unique.getFieldNames().add("number");

		FluentDocumentUniqueConstraint fluentUnique = new FluentDocumentUniqueConstraint().from(unique);
		assertThat(fluentUnique.get().getName(), is("uqOrder"));
		assertEquals(1, fluentUnique.get().getFieldReferences().size());

		FluentCollectionUniqueConstraint collectionUnique = new FluentCollectionUniqueConstraint().from(unique);
		assertEquals(1, collectionUnique.get().getFieldNames().size());
		collectionUnique.removeFieldName("number").clearFieldNames();
		assertTrue(collectionUnique.get().getFieldNames().isEmpty());
	}

	@Test
	void dynamicWrappingConstructorPreservesInstance() {
		Dynamic existing = new Dynamic();
		FluentDynamic fluent = new FluentDynamic(existing);
		assertSame(existing, fluent.get());
	}

	@Test
	void fluentDocumentUniqueConstraintRemoveFieldName() {
		FluentDocumentUniqueConstraint fluentUnique = new FluentDocumentUniqueConstraint().addFieldName("number");
		fluentUnique.removeFieldName("number");
		assertTrue(fluentUnique.get().getFieldReferences().isEmpty());
	}

	@Test
	void fluentDocumentUniqueConstraintClearFieldNames() {
		FluentDocumentUniqueConstraint fluentUnique = new FluentDocumentUniqueConstraint().addFieldName("a").addFieldName("b");
		fluentUnique.clearFieldNames();
		assertTrue(fluentUnique.get().getFieldReferences().isEmpty());
	}

	@Test
	void fromDocumentWithImageAttributeCopiesIt() {
		DocumentImpl doc = new DocumentImpl();
		doc.setOwningModuleName("test");
		Image image = new Image();
		image.setName("photo");
		doc.putAttribute(image);
		FluentDocument fluent = new FluentDocument().from(doc);
		assertFalse(fluent.get().getAttributes().isEmpty());
		assertEquals("photo", fluent.get().getAttributes().get(0).getName());
	}

	@Test
	void fromDocumentWithConditionCopiesIt() {
		DocumentImpl doc = new DocumentImpl();
		doc.setOwningModuleName("test");
		ConditionMetaData cond = new ConditionMetaData();
		doc.getConditions().put("active", cond);
		FluentDocument fluent = new FluentDocument().from(doc);
		assertEquals(1, fluent.get().getConditions().size());
	}

	@Test
	void fromDocumentWithUniqueConstraintCopiesIt() {
		DocumentImpl doc = new DocumentImpl();
		doc.setOwningModuleName("test");
		UniqueConstraintImpl constraint = new UniqueConstraintImpl();
		constraint.setName("uqTest");
		doc.putUniqueConstraint(constraint);
		FluentDocument fluent = new FluentDocument().from(doc);
		assertEquals(1, fluent.get().getUniqueConstraints().size());
	}

	@Test
	void fluentAssociationDatabaseIndexFalseSetsBooleanFalse() {
		FluentAssociation fa = new FluentAssociation();
		fa.databaseIndex(false);
		assertEquals(Boolean.FALSE, fa.get().getDatabaseIndex());
	}

	@Test
	void fluentParentDocumentFromWithNullIndexCallsDatabaseIndexTrue() {
		ParentDocument parent = new ParentDocument();
		parent.setParentDocumentName("Base");
		// getDatabaseIndex() is null → databaseIndex(true) branch in from()
		FluentParentDocument result = new FluentParentDocument().from(parent);
		assertEquals(Boolean.TRUE, result.get().getDatabaseIndex());
	}

	@Test
	void fromDocumentWithUnknownAttributeTypeThrowsIllegalState() {
		DocumentImpl doc = new DocumentImpl();
		AbstractAttribute unknown = new AbstractAttribute() {
			@Override public boolean isScalar() { return false; }
			@Override public boolean isPersistent() { return false; }
			@Override public boolean isRequired() { return false; }
			@Override public DomainType getDomainType() { return null; }
		};
		unknown.setName("unknown");
		doc.putAttribute(unknown);
		assertThrows(IllegalStateException.class, () -> new FluentDocument().from(doc));
	}
}