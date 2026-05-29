package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.InterfaceImpl;
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
import org.skyve.impl.metadata.model.document.field.LongInteger;
import org.skyve.impl.metadata.model.document.field.Markup;
import org.skyve.impl.metadata.model.document.field.Memo;
import org.skyve.impl.metadata.model.document.field.Text;
import org.skyve.impl.metadata.model.document.field.Time;
import org.skyve.impl.metadata.model.document.field.Timestamp;
import org.skyve.impl.metadata.repository.document.BizKey;
import org.skyve.impl.metadata.repository.document.ConditionMetaData;
import org.skyve.impl.metadata.repository.document.DocumentMetaData;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.Sensitivity;
import org.skyve.metadata.model.Dynamic;
import org.skyve.metadata.model.Extends;
import org.skyve.metadata.model.Persistent;
import org.skyve.metadata.model.document.Association;
import org.skyve.metadata.model.document.Collection;
import org.skyve.metadata.model.document.Condition;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.Interface;
import org.skyve.metadata.model.document.UniqueConstraint;

/**
 * Provides a fluent builder for FluentDocument metadata.
 */
public class FluentDocument {
	private DocumentMetaData document = null;
	
	/**
	 * Creates a fluent builder instance.
	 */
	public FluentDocument() {
		document = new DocumentMetaData();
	}

	/**
	 * Creates a fluent builder instance.
	 */
	public FluentDocument(DocumentMetaData document) {
		this.document = document;
	}

	/**
	 * Copies a document definition into this builder, including interfaces, attributes,
	 * conditions, and unique constraints.
	 *
	 * <p>Side effects: clears/overwrites state in this builder as values are applied.
	 * Unknown attribute implementations cause an {@link IllegalStateException}.
	 */
	public FluentDocument from(@SuppressWarnings("hiding") Document document) {
		DocumentImpl impl = (DocumentImpl) document;
		name(document.getName());
		abstractDocument(document.isAbstract());
		documentation(document.getDocumentation());
		
		Extends inherits = document.getExtends();
		if (inherits != null) {
			extendsDocument(inherits.getDocumentName());
		}

		String parentDocumentName = document.getParentDocumentName();
		if (parentDocumentName != null) {
			FluentParentDocument parent = new FluentParentDocument().parentDocumentName(parentDocumentName);
			Boolean b = impl.getParentDatabaseIndex();
			if (b != null) {
				parent.databaseIndex(b.booleanValue());
			}
			parentDocument(parent);
		}
		
		Persistent persistent = document.getPersistent();
		if (persistent != null) {
			persistent(new FluentPersistent().from(persistent));
		}
		
		Dynamic dynamic = document.getDynamism();
		if (dynamic != null) {
			dynamic(new FluentDynamic().from(dynamic));
		}

		singularAlias(document.getSingularAlias());
		pluralAlias(document.getPluralAlias());
		description(document.getDescription());
		bizKeyExpression(document.getBizKeyExpression());
		iconStyleClass(document.getIconStyleClass());
		icon16x16RelativeFilePath(document.getIcon16x16RelativeFileName());
		icon32x32RelativeFilePath(document.getIcon32x32RelativeFileName());
		audited(document.isAudited());
		
		// Populate interfaces
		for (Interface i : document.getInterfaces()) {
			addImplementingInterface(i.getInterfaceName());
		}
		
		// Populate attributes
		for (Attribute attribute : document.getAttributes()) {
			if (attribute instanceof Text text) {
				addAttribute(new FluentText().from(text));
			}
			else if (attribute instanceof org.skyve.impl.metadata.model.document.field.Boolean bool) {
				addAttribute(new FluentBoolean().from(bool));
			}
			else if (attribute instanceof Enumeration enumeration) {
				addAttribute(new FluentEnumeration().from(enumeration));
			}
			else if (attribute instanceof Markup markup) {
				addAttribute(new FluentMarkup().from(markup));
			}
			else if (attribute instanceof Memo memo) {
				addAttribute(new FluentMemo().from(memo));
			}
			else if (attribute instanceof Date date) {
				addAttribute(new FluentDate().from(date));
			}
			else if (attribute instanceof org.skyve.impl.metadata.model.document.field.Integer integer) {
				addAttribute(new FluentInteger().from(integer));
			}
			else if (attribute instanceof Association association) {
				addAttribute(new FluentAssociation().from(association));
			}
			else if (attribute instanceof Collection collection) {
				addAttribute(new FluentCollection().from(collection));
			}
			else if (attribute instanceof LongInteger longInteger) {
				addAttribute(new FluentLongInteger().from(longInteger));
			}
			else if (attribute instanceof Decimal2 decimal) {
				addAttribute(new FluentDecimal2().from(decimal));
			}
			else if (attribute instanceof Decimal5 decimal) {
				addAttribute(new FluentDecimal5().from(decimal));
			}
			else if (attribute instanceof Decimal10 decimal) {
				addAttribute(new FluentDecimal10().from(decimal));
			}
			else if (attribute instanceof Time time) {
				addAttribute(new FluentTime().from(time));
			}
			else if (attribute instanceof DateTime dateTime) {
				addAttribute(new FluentDateTime().from(dateTime));
			}
			else if (attribute instanceof Timestamp timestamp) {
				addAttribute(new FluentTimestamp().from(timestamp));
			}
			else if (attribute instanceof Colour colour) {
				addAttribute(new FluentColour().from(colour));
			}
			else if (attribute instanceof Content content) {
				addAttribute(new FluentContent().from(content));
			}
			else if (attribute instanceof Image image) {
				addAttribute(new FluentImage().from(image));
			}
			else if (attribute instanceof Geometry geometry) {
				addAttribute(new FluentGeometry().from(geometry));
			}
			else if (attribute instanceof Id id) {
				addAttribute(new FluentId().from(id));
			}
			else if (attribute instanceof InverseOne inverseOne) {
				addAttribute(new FluentInverseOne().from(inverseOne));
			}
			else if (attribute instanceof InverseMany inverseMany) {
				addAttribute(new FluentInverseMany().from(inverseMany));
			}
			else {
				throw new IllegalStateException(attribute + " not catered for");
			}
		}
		
		// Populate Conditions
		for (String conditionName : document.getConditionNames()) {
			Condition condition = document.getCondition(conditionName);
			addCondition(new FluentCondition().from(conditionName, condition));
		}
		
		// Populate Unique Constraints
		for (UniqueConstraint constraint : document.getUniqueConstraints()) {
			addUniqueConstraint(new FluentDocumentUniqueConstraint().from(constraint));
		}
		
		return this;
	}

	/**
	 * Updates metadata on this fluent builder.
	 */
	public FluentDocument name(String name) {
		document.setName(name);
		return this;
	}
	
	/**
	 * Updates metadata on this fluent builder.
	 */
	public FluentDocument abstractDocument(boolean abstractDocument) {
		document.setAbstract(abstractDocument ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	/**
	 * Updates metadata on this fluent builder.
	 */
	public FluentDocument documentation(String documentation) {
		document.setDocumentation(documentation);
		return this;
	}

	/**
	 * Sets the inherited base document reference.
	 *
	 * <p>Side effects: replaces any existing {@code extends} definition.
	 */
	public FluentDocument extendsDocument(String baseDocumentName) {
		Extends inherits = new Extends();
		inherits.setDocumentName(baseDocumentName);
		document.setExtends(inherits);
		return this;
	}

	/**
	 * Updates metadata on this fluent builder.
	 */
	public FluentDocument parentDocument(FluentParentDocument parent) {
		document.setParentDocument(parent.get());
		return this;
	}

	/**
	 * Updates metadata on this fluent builder.
	 */
	public FluentDocument persistent(FluentPersistent persistent) {
		document.setPersistent(persistent.get());
		return this;
	}
	
	/**
	 * Updates metadata on this fluent builder.
	 */
	public FluentDocument dynamic(FluentDynamic dynamic) {
		document.setDynamic(dynamic.get());
		return this;
	}

	/**
	 * Updates metadata on this fluent builder.
	 */
	public FluentDocument singularAlias(String singularAlias) {
		document.setSingularAlias(singularAlias);
		return this;
	}

	/**
	 * Updates metadata on this fluent builder.
	 */
	public FluentDocument pluralAlias(String pluralAlias) {
		document.setPluralAlias(pluralAlias);
		return this;
	}

	/**
	 * Updates metadata on this fluent builder.
	 */
	public FluentDocument description(String description) {
		document.setDescription(description);
		return this;
	}

	/**
	 * Sets the BizKey expression, creating the BizKey block when absent.
	 */
	public FluentDocument bizKeyExpression(String expression) {
		BizKey bizKey = document.getBizKey();
		if (bizKey == null) {
			bizKey = new BizKey();
			document.setBizKey(bizKey);
		}
		bizKey.setExpression(expression);
		return this;
	}

	/**
	 * Sets BizKey sensitivity, creating the BizKey block when absent.
	 */
	public FluentDocument bizKeySensitivity(Sensitivity sensitivity) {
		BizKey bizKey = document.getBizKey();
		if (bizKey == null) {
			bizKey = new BizKey();
			document.setBizKey(bizKey);
		}
		bizKey.setSensitivity(sensitivity);
		return this;
	}

	/**
	 * Updates metadata on this fluent builder.
	 */
	public FluentDocument iconStyleClass(String iconStyleClass) {
		document.setIconStyleClass(iconStyleClass);
		return this;
	}
	
	/**
	 * Updates metadata on this fluent builder.
	 */
	public FluentDocument icon16x16RelativeFilePath(String icon16x16RelativeFilePath) {
		document.setIcon16x16RelativeFilePath(icon16x16RelativeFilePath);
		return this;
	}
	
	/**
	 * Updates metadata on this fluent builder.
	 */
	public FluentDocument icon32x32RelativeFilePath(String icon32x32RelativeFilePath) {
		document.setIcon32x32RelativeFilePath(icon32x32RelativeFilePath);
		return this;
	}
	
	/**
	 * Updates metadata on this fluent builder.
	 */
	public FluentDocument audited(boolean audited) {
		document.setAudited(audited ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}
	/**
	 * Adds a metadata definition to this builder.
	 */
	public FluentDocument addImplementingInterface(String fullyQualifiedInterfaceClassName) {
		InterfaceImpl i = new InterfaceImpl();
		i.setInterfaceName(fullyQualifiedInterfaceClassName);
		document.getImplements().add(i);
		return this;
	}
	/**
	 * Removes matching metadata definitions from this builder.
	 */
	public FluentDocument removeImplementingInterface(String fullyQualifiedInterfaceClassName) {
		document.getImplements().removeIf(i -> fullyQualifiedInterfaceClassName.equals(i.getInterfaceName()));
		return this;
	}

	/**
	 * Adds an attribute definition to the document.
	 *
	 * <p>Side effects: appends to the current attribute list; ordering is preserved.
	 */
	public FluentDocument addAttribute(FluentAttribute<?> attribute) {
		document.getAttributes().add(attribute.get());
		return this;
	}
	/**
	 * Adds a metadata definition to this builder.
	 */
	public FluentDocument addText(FluentText text) {
		return addAttribute(text);
	}
	
	private Attribute findAttribute(String name) {
		return document.getAttributes().stream().filter(a -> name.equals(a.getName())).findAny().orElse(null);
	}
	/**
	 * Finds a matching definition by key and returns null when no match exists.
	 */
	public FluentText findText(String name) {
		Text result = (Text) findAttribute(name);
		if (result != null) {
			return new FluentText(result);
		}
		return null;
	}
	/**
	 * Adds a metadata definition to this builder.
	 */
	public FluentDocument addDate(FluentDate date) {
		return addAttribute(date);
	}
	/**
	 * Finds a matching definition by key and returns null when no match exists.
	 */
	public FluentDate findDate(String name) {
		Date result = (Date) findAttribute(name);
		if (result != null) {
			return new FluentDate(result);
		}
		return null;
	}
	/**
	 * Adds a metadata definition to this builder.
	 */
	public FluentDocument addTime(FluentTime time) {
		return addAttribute(time);
	}
	/**
	 * Finds a matching definition by key and returns null when no match exists.
	 */
	public FluentTime findTime(String name) {
		Time result = (Time) findAttribute(name);
		if (result != null) {
			return new FluentTime(result);
		}
		return null;
	}
	/**
	 * Adds a metadata definition to this builder.
	 */
	public FluentDocument addDateTime(FluentDateTime dateTime) {
		return addAttribute(dateTime);
	}
	/**
	 * Finds a matching definition by key and returns null when no match exists.
	 */
	public FluentDateTime findDateTime(String name) {
		DateTime result = (DateTime) findAttribute(name);
		if (result != null) {
			return new FluentDateTime(result);
		}
		return null;
	}
	/**
	 * Adds a metadata definition to this builder.
	 */
	public FluentDocument addTimestamp(FluentTimestamp timestamp) {
		return addAttribute(timestamp);
	}
	/**
	 * Finds a matching definition by key and returns null when no match exists.
	 */
	public FluentTimestamp findTimestamp(String name) {
		Timestamp result = (Timestamp) findAttribute(name);
		if (result != null) {
			return new FluentTimestamp(result);
		}
		return null;
	}
	/**
	 * Adds a metadata definition to this builder.
	 */
	public FluentDocument addInteger(FluentInteger integer) {
		return addAttribute(integer);
	}
	/**
	 * Finds a matching definition by key and returns null when no match exists.
	 */
	public FluentInteger findInteger(String name) {
		org.skyve.impl.metadata.model.document.field.Integer result = (org.skyve.impl.metadata.model.document.field.Integer) findAttribute(name);
		if (result != null) {
			return new FluentInteger(result);
		}
		return null;
	}
	/**
	 * Adds a metadata definition to this builder.
	 */
	public FluentDocument addLongInteger(FluentLongInteger longInteger) {
		return addAttribute(longInteger);
	}
	/**
	 * Finds a matching definition by key and returns null when no match exists.
	 */
	public FluentLongInteger findLongInteger(String name) {
		LongInteger result = (LongInteger) findAttribute(name);
		if (result != null) {
			return new FluentLongInteger(result);
		}
		return null;
	}
	/**
	 * Adds a metadata definition to this builder.
	 */
	public FluentDocument addDecimal2(FluentDecimal2 decimal) {
		return addAttribute(decimal);
	}
	/**
	 * Finds a matching definition by key and returns null when no match exists.
	 */
	public FluentDecimal2 findDecimal2(String name) {
		Decimal2 result = (Decimal2) findAttribute(name);
		if (result != null) {
			return new FluentDecimal2(result);
		}
		return null;
	}
	/**
	 * Adds a metadata definition to this builder.
	 */
	public FluentDocument addDecimal5(FluentDecimal5 decimal) {
		return addAttribute(decimal);
	}
	/**
	 * Finds a matching definition by key and returns null when no match exists.
	 */
	public FluentDecimal5 findDecimal5(String name) {
		Decimal5 result = (Decimal5) findAttribute(name);
		if (result != null) {
			return new FluentDecimal5(result);
		}
		return null;
	}
	/**
	 * Adds a metadata definition to this builder.
	 */
	public FluentDocument addDecimal10(FluentDecimal10 decimal) {
		return addAttribute(decimal);
	}
	/**
	 * Finds a matching definition by key and returns null when no match exists.
	 */
	public FluentDecimal10 findDecimal10(String name) {
		Decimal10 result = (Decimal10) findAttribute(name);
		if (result != null) {
			return new FluentDecimal10(result);
		}
		return null;
	}
	/**
	 * Adds a metadata definition to this builder.
	 */
	public FluentDocument addBoolean(FluentBoolean bool) {
		return addAttribute(bool);
	}
	/**
	 * Finds a matching definition by key and returns null when no match exists.
	 */
	public FluentBoolean findBoolean(String name) {
		org.skyve.impl.metadata.model.document.field.Boolean result = (org.skyve.impl.metadata.model.document.field.Boolean) findAttribute(name);
		if (result != null) {
			return new FluentBoolean(result);
		}
		return null;
	}
	/**
	 * Adds a metadata definition to this builder.
	 */
	public FluentDocument addEnumeration(FluentEnumeration enumeration) {
		return addAttribute(enumeration);
	}
	/**
	 * Finds a matching definition by key and returns null when no match exists.
	 */
	public FluentEnumeration findEnumeration(String name) {
		Enumeration result = (Enumeration) findAttribute(name);
		if (result != null) {
			return new FluentEnumeration(result);
		}
		return null;
	}
	/**
	 * Adds a metadata definition to this builder.
	 */
	public FluentDocument addMemo(FluentMemo memo) {
		return addAttribute(memo);
	}
	/**
	 * Finds a matching definition by key and returns null when no match exists.
	 */
	public FluentMemo findMemo(String name) {
		Memo result = (Memo) findAttribute(name);
		if (result != null) {
			return new FluentMemo(result);
		}
		return null;
	}
	/**
	 * Adds a metadata definition to this builder.
	 */
	public FluentDocument addMarkup(FluentMarkup markup) {
		return addAttribute(markup);
	}
	/**
	 * Finds a matching definition by key and returns null when no match exists.
	 */
	public FluentMarkup findMarkup(String name) {
		Markup result = (Markup) findAttribute(name);
		if (result != null) {
			return new FluentMarkup(result);
		}
		return null;
	}
	/**
	 * Adds a metadata definition to this builder.
	 */
	public FluentDocument addColour(FluentColour colour) {
		return addAttribute(colour);
	}
	/**
	 * Finds a matching definition by key and returns null when no match exists.
	 */
	public FluentColour findColour(String name) {
		Colour result = (Colour) findAttribute(name);
		if (result != null) {
			return new FluentColour(result);
		}
		return null;
	}
	/**
	 * Adds a metadata definition to this builder.
	 */
	public FluentDocument addContent(FluentContent content) {
		return addAttribute(content);
	}
	/**
	 * Finds a matching definition by key and returns null when no match exists.
	 */
	public FluentContent findContent(String name) {
		Content result = (Content) findAttribute(name);
		if (result != null) {
			return new FluentContent(result);
		}
		return null;
	}
	/**
	 * Adds a metadata definition to this builder.
	 */
	public FluentDocument addImage(FluentImage image) {
		return addAttribute(image);
	}
	/**
	 * Finds a matching definition by key and returns null when no match exists.
	 */
	public FluentImage findImage(String name) {
		Image result = (Image) findAttribute(name);
		if (result != null) {
			return new FluentImage(result);
		}
		return null;
	}
	/**
	 * Adds a metadata definition to this builder.
	 */
	public FluentDocument addGeometry(FluentGeometry geometry) {
		return addAttribute(geometry);
	}
	/**
	 * Finds a matching definition by key and returns null when no match exists.
	 */
	public FluentGeometry findGeometry(String name) {
		Geometry result = (Geometry) findAttribute(name);
		if (result != null) {
			return new FluentGeometry(result);
		}
		return null;
	}
	/**
	 * Adds a metadata definition to this builder.
	 */
	public FluentDocument addId(FluentId id) {
		return addAttribute(id);
	}
	/**
	 * Finds a matching definition by key and returns null when no match exists.
	 */
	public FluentId findId(String name) {
		Id result = (Id) findAttribute(name);
		if (result != null) {
			return new FluentId(result);
		}
		return null;
	}
	/**
	 * Adds a metadata definition to this builder.
	 */
	public FluentDocument addAssociation(FluentAssociation association) {
		return addAttribute(association);
	}
	/**
	 * Finds a matching definition by key and returns null when no match exists.
	 */
	public FluentAssociation findAssociation(String name) {
		AssociationImpl result = (AssociationImpl) findAttribute(name);
		if (result != null) {
			return new FluentAssociation(result);
		}
		return null;
	}
	/**
	 * Adds a metadata definition to this builder.
	 */
	public FluentDocument addCollection(FluentCollection collection) {
		return addAttribute(collection);
	}
	/**
	 * Finds a matching definition by key and returns null when no match exists.
	 */
	public FluentCollection findCollection(String name) {
		CollectionImpl result = (CollectionImpl) findAttribute(name);
		if (result != null) {
			return new FluentCollection(result);
		}
		return null;
	}
	/**
	 * Adds a metadata definition to this builder.
	 */
	public FluentDocument addInverseOne(FluentInverseOne inverseOne) {
		return addAttribute(inverseOne);
	}
	/**
	 * Finds a matching definition by key and returns null when no match exists.
	 */
	public FluentInverseOne findInverseOne(String name) {
		InverseOne result = (InverseOne) findAttribute(name);
		if (result != null) {
			return new FluentInverseOne(result);
		}
		return null;
	}
	/**
	 * Adds a metadata definition to this builder.
	 */
	public FluentDocument addInverseMany(FluentInverseMany inverseMany) {
		return addAttribute(inverseMany);
	}
	/**
	 * Finds a matching definition by key and returns null when no match exists.
	 */
	public FluentInverseMany findInverseMany(String name) {
		InverseMany result = (InverseMany) findAttribute(name);
		if (result != null) {
			return new FluentInverseMany(result);
		}
		return null;
	}

	/**
	 * Removes all attributes whose name matches {@code name}.
	 */
	public FluentDocument removeAttribute(String name) {
		document.getAttributes().removeIf(a -> name.equals(a.getName()));
		return this;
	}

	/**
	 * Removes all configured attributes.
	 */
	public FluentDocument clearAttributes() {
		document.getAttributes().clear();
		return this;
	}

	/**
	 * Appends a condition definition.
	 */
	public FluentDocument addCondition(FluentCondition condition) {
		document.getConditions().add(condition.get());
		return this;
	}

	/**
	 * Removes conditions that match {@code name}.
	 */
	public FluentDocument removeCondition(String name) {
		document.getConditions().removeIf(c -> name.equals(c.getName()));
		return this;
	}

	/**
	 * Removes all configured conditions.
	 */
	public FluentDocument clearConditions() {
		document.getConditions().clear();
		return this;
	}
	
	/**
	 * Finds a condition by name.
	 *
	 * @return a fluent wrapper around the first matching condition, or {@code null} when absent
	 */
	public FluentCondition findCondition(String name) {
		ConditionMetaData result = document.getConditions().stream().filter(c -> name.equals(c.getName())).findAny().orElse(null);
		if (result != null) {
			return new FluentCondition(result);
		}
		return null;
	}
	/**
	 * Appends a document-level unique-constraint definition.
	 */
	public FluentDocument addUniqueConstraint(FluentDocumentUniqueConstraint constraint) {
		document.getUniqueConstraints().add(constraint.get());
		return this;
	}

	/**
	 * Removes document-level unique constraints that match {@code name}.
	 */
	public FluentDocument removeUniqueConstraint(String name) {
		document.getUniqueConstraints().removeIf(c -> name.equals(c.getName()));
		return this;
	}

	/**
	 * Removes all document-level unique constraints.
	 */
	public FluentDocument clearUniqueConstraint() {
		document.getUniqueConstraints().clear();
		return this;
	}

	/**
	 * Finds a document-level unique constraint by name.
	 *
	 * @return a fluent wrapper around the first matching constraint, or {@code null} when absent
	 */
	public FluentDocumentUniqueConstraint findUniqueConstraint(String name) {
		org.skyve.impl.metadata.repository.document.UniqueConstraint result = document.getUniqueConstraints().stream().filter(c -> name.equals(c.getName())).findAny().orElse(null);
		if (result != null) {
			return new FluentDocumentUniqueConstraint(result);
		}
		return null;
	}
	
	/**
	 * Returns the mutable document metadata instance being built.
	 */
	public DocumentMetaData get() {
		return document;
	}
}
