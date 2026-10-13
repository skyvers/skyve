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

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

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
	public FluentDocument(@Nonnull DocumentMetaData document) {
		this.document = document;
	}

	/**
	 * Copies a document definition into this builder, including interfaces, attributes,
	 * conditions, and unique constraints.
	 *
	 * <p>Side effects: clears/overwrites state in this builder as values are applied.
	 * Unknown attribute implementations cause an {@link IllegalStateException}.
	 */
	@SuppressWarnings("java:S3776") // Complexity OK
	public @Nonnull FluentDocument from(@SuppressWarnings("hiding") @Nonnull Document document) {
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
		
		document.getProperties().entrySet().forEach(p -> putProperty(p.getKey(), p.getValue()));

		return this;
	}

	/**
	 * Updates metadata on this fluent builder.
	 */
	public @Nonnull FluentDocument name(@Nullable String name) {
		document.setName(name);
		return this;
	}
	
	/**
	 * Updates metadata on this fluent builder.
	 */
	public @Nonnull FluentDocument abstractDocument(boolean abstractDocument) {
		document.setAbstract(abstractDocument ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	/**
	 * Updates metadata on this fluent builder.
	 */
	public @Nonnull FluentDocument documentation(@Nullable String documentation) {
		document.setDocumentation(documentation);
		return this;
	}

	/**
	 * Sets the inherited base document reference.
	 *
	 * <p>Side effects: replaces any existing {@code extends} definition.
	 */
	public @Nonnull FluentDocument extendsDocument(@Nullable String baseDocumentName) {
		Extends inherits = new Extends();
		inherits.setDocumentName(baseDocumentName);
		document.setExtends(inherits);
		return this;
	}

	/**
	 * Updates metadata on this fluent builder.
	 */
	public @Nonnull FluentDocument parentDocument(@Nonnull FluentParentDocument parent) {
		document.setParentDocument(parent.get());
		return this;
	}

	/**
	 * Updates metadata on this fluent builder.
	 */
	public @Nonnull FluentDocument persistent(@Nonnull FluentPersistent persistent) {
		document.setPersistent(persistent.get());
		return this;
	}
	
	/**
	 * Updates metadata on this fluent builder.
	 */
	public @Nonnull FluentDocument dynamic(@Nonnull FluentDynamic dynamic) {
		document.setDynamic(dynamic.get());
		return this;
	}

	/**
	 * Updates metadata on this fluent builder.
	 */
	public @Nonnull FluentDocument singularAlias(@Nullable String singularAlias) {
		document.setSingularAlias(singularAlias);
		return this;
	}

	/**
	 * Updates metadata on this fluent builder.
	 */
	public @Nonnull FluentDocument pluralAlias(@Nullable String pluralAlias) {
		document.setPluralAlias(pluralAlias);
		return this;
	}

	/**
	 * Updates metadata on this fluent builder.
	 */
	public @Nonnull FluentDocument description(@Nullable String description) {
		document.setDescription(description);
		return this;
	}

	/**
	 * Sets the BizKey expression, creating the BizKey block when absent.
	 */
	public @Nonnull FluentDocument bizKeyExpression(@Nullable String expression) {
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
	public @Nonnull FluentDocument bizKeySensitivity(@Nullable Sensitivity sensitivity) {
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
	public @Nonnull FluentDocument iconStyleClass(@Nullable String iconStyleClass) {
		document.setIconStyleClass(iconStyleClass);
		return this;
	}
	
	/**
	 * Updates metadata on this fluent builder.
	 */
	public @Nonnull FluentDocument icon16x16RelativeFilePath(@Nullable String icon16x16RelativeFilePath) {
		document.setIcon16x16RelativeFilePath(icon16x16RelativeFilePath);
		return this;
	}
	
	/**
	 * Updates metadata on this fluent builder.
	 */
	public @Nonnull FluentDocument icon32x32RelativeFilePath(@Nullable String icon32x32RelativeFilePath) {
		document.setIcon32x32RelativeFilePath(icon32x32RelativeFilePath);
		return this;
	}
	
	/**
	 * Updates metadata on this fluent builder.
	 */
	public @Nonnull FluentDocument audited(boolean audited) {
		document.setAudited(audited ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}
	/**
	 * Adds a metadata definition to this builder.
	 */
	public @Nonnull FluentDocument addImplementingInterface(@Nullable String fullyQualifiedInterfaceClassName) {
		InterfaceImpl i = new InterfaceImpl();
		i.setInterfaceName(fullyQualifiedInterfaceClassName);
		document.getImplements().add(i);
		return this;
	}
	/**
	 * Removes matching metadata definitions from this builder.
	 */
	public @Nonnull FluentDocument removeImplementingInterface(@Nonnull String fullyQualifiedInterfaceClassName) {
		document.getImplements().removeIf(i -> fullyQualifiedInterfaceClassName.equals(i.getInterfaceName()));
		return this;
	}

	/**
	 * Adds an attribute definition to the document.
	 *
	 * <p>Side effects: appends to the current attribute list; ordering is preserved.
	 */
	public @Nonnull FluentDocument addAttribute(@Nonnull FluentAttribute<?> attribute) {
		document.getAttributes().add(attribute.get());
		return this;
	}
	/**
	 * Adds a metadata definition to this builder.
	 */
	public @Nonnull FluentDocument addText(@Nonnull FluentText text) {
		return addAttribute(text);
	}
	
	private @Nullable Attribute findAttribute(@Nonnull String name) {
		return document.getAttributes().stream().filter(a -> name.equals(a.getName())).findAny().orElse(null);
	}
	/**
	 * Finds a matching definition by key and returns null when no match exists.
	 */
	public @Nullable FluentText findText(@Nonnull String name) {
		Text result = (Text) findAttribute(name);
		if (result != null) {
			return new FluentText(result);
		}
		return null;
	}
	/**
	 * Adds a metadata definition to this builder.
	 */
	public @Nonnull FluentDocument addDate(@Nonnull FluentDate date) {
		return addAttribute(date);
	}
	/**
	 * Finds a matching definition by key and returns null when no match exists.
	 */
	public @Nullable FluentDate findDate(@Nonnull String name) {
		Date result = (Date) findAttribute(name);
		if (result != null) {
			return new FluentDate(result);
		}
		return null;
	}
	/**
	 * Adds a metadata definition to this builder.
	 */
	public @Nonnull FluentDocument addTime(@Nonnull FluentTime time) {
		return addAttribute(time);
	}
	/**
	 * Finds a matching definition by key and returns null when no match exists.
	 */
	public @Nullable FluentTime findTime(@Nonnull String name) {
		Time result = (Time) findAttribute(name);
		if (result != null) {
			return new FluentTime(result);
		}
		return null;
	}
	/**
	 * Adds a metadata definition to this builder.
	 */
	public @Nonnull FluentDocument addDateTime(@Nonnull FluentDateTime dateTime) {
		return addAttribute(dateTime);
	}
	/**
	 * Finds a matching definition by key and returns null when no match exists.
	 */
	public @Nullable FluentDateTime findDateTime(@Nonnull String name) {
		DateTime result = (DateTime) findAttribute(name);
		if (result != null) {
			return new FluentDateTime(result);
		}
		return null;
	}
	/**
	 * Adds a metadata definition to this builder.
	 */
	public @Nonnull FluentDocument addTimestamp(@Nonnull FluentTimestamp timestamp) {
		return addAttribute(timestamp);
	}
	/**
	 * Finds a matching definition by key and returns null when no match exists.
	 */
	public @Nullable FluentTimestamp findTimestamp(@Nonnull String name) {
		Timestamp result = (Timestamp) findAttribute(name);
		if (result != null) {
			return new FluentTimestamp(result);
		}
		return null;
	}
	/**
	 * Adds a metadata definition to this builder.
	 */
	public @Nonnull FluentDocument addInteger(@Nonnull FluentInteger integer) {
		return addAttribute(integer);
	}
	/**
	 * Finds a matching definition by key and returns null when no match exists.
	 */
	public @Nullable FluentInteger findInteger(@Nonnull String name) {
		org.skyve.impl.metadata.model.document.field.Integer result = (org.skyve.impl.metadata.model.document.field.Integer) findAttribute(name);
		if (result != null) {
			return new FluentInteger(result);
		}
		return null;
	}
	/**
	 * Adds a metadata definition to this builder.
	 */
	public @Nonnull FluentDocument addLongInteger(@Nonnull FluentLongInteger longInteger) {
		return addAttribute(longInteger);
	}
	/**
	 * Finds a matching definition by key and returns null when no match exists.
	 */
	public @Nullable FluentLongInteger findLongInteger(@Nonnull String name) {
		LongInteger result = (LongInteger) findAttribute(name);
		if (result != null) {
			return new FluentLongInteger(result);
		}
		return null;
	}
	/**
	 * Adds a metadata definition to this builder.
	 */
	public @Nonnull FluentDocument addDecimal2(@Nonnull FluentDecimal2 decimal) {
		return addAttribute(decimal);
	}
	/**
	 * Finds a matching definition by key and returns null when no match exists.
	 */
	public @Nullable FluentDecimal2 findDecimal2(@Nonnull String name) {
		Decimal2 result = (Decimal2) findAttribute(name);
		if (result != null) {
			return new FluentDecimal2(result);
		}
		return null;
	}
	/**
	 * Adds a metadata definition to this builder.
	 */
	public @Nonnull FluentDocument addDecimal5(@Nonnull FluentDecimal5 decimal) {
		return addAttribute(decimal);
	}
	/**
	 * Finds a matching definition by key and returns null when no match exists.
	 */
	public @Nullable FluentDecimal5 findDecimal5(@Nonnull String name) {
		Decimal5 result = (Decimal5) findAttribute(name);
		if (result != null) {
			return new FluentDecimal5(result);
		}
		return null;
	}
	/**
	 * Adds a metadata definition to this builder.
	 */
	public @Nonnull FluentDocument addDecimal10(@Nonnull FluentDecimal10 decimal) {
		return addAttribute(decimal);
	}
	/**
	 * Finds a matching definition by key and returns null when no match exists.
	 */
	public @Nullable FluentDecimal10 findDecimal10(@Nonnull String name) {
		Decimal10 result = (Decimal10) findAttribute(name);
		if (result != null) {
			return new FluentDecimal10(result);
		}
		return null;
	}
	/**
	 * Adds a metadata definition to this builder.
	 */
	public @Nonnull FluentDocument addBoolean(@Nonnull FluentBoolean bool) {
		return addAttribute(bool);
	}
	/**
	 * Finds a matching definition by key and returns null when no match exists.
	 */
	public @Nullable FluentBoolean findBoolean(@Nonnull String name) {
		org.skyve.impl.metadata.model.document.field.Boolean result = (org.skyve.impl.metadata.model.document.field.Boolean) findAttribute(name);
		if (result != null) {
			return new FluentBoolean(result);
		}
		return null;
	}
	/**
	 * Adds a metadata definition to this builder.
	 */
	public @Nonnull FluentDocument addEnumeration(@Nonnull FluentEnumeration enumeration) {
		return addAttribute(enumeration);
	}
	/**
	 * Finds a matching definition by key and returns null when no match exists.
	 */
	public @Nullable FluentEnumeration findEnumeration(@Nonnull String name) {
		Enumeration result = (Enumeration) findAttribute(name);
		if (result != null) {
			return new FluentEnumeration(result);
		}
		return null;
	}
	/**
	 * Adds a metadata definition to this builder.
	 */
	public @Nonnull FluentDocument addMemo(@Nonnull FluentMemo memo) {
		return addAttribute(memo);
	}
	/**
	 * Finds a matching definition by key and returns null when no match exists.
	 */
	public @Nullable FluentMemo findMemo(@Nonnull String name) {
		Memo result = (Memo) findAttribute(name);
		if (result != null) {
			return new FluentMemo(result);
		}
		return null;
	}
	/**
	 * Adds a metadata definition to this builder.
	 */
	public @Nonnull FluentDocument addMarkup(@Nonnull FluentMarkup markup) {
		return addAttribute(markup);
	}
	/**
	 * Finds a matching definition by key and returns null when no match exists.
	 */
	public @Nullable FluentMarkup findMarkup(@Nonnull String name) {
		Markup result = (Markup) findAttribute(name);
		if (result != null) {
			return new FluentMarkup(result);
		}
		return null;
	}
	/**
	 * Adds a metadata definition to this builder.
	 */
	public @Nonnull FluentDocument addColour(@Nonnull FluentColour colour) {
		return addAttribute(colour);
	}
	/**
	 * Finds a matching definition by key and returns null when no match exists.
	 */
	public @Nullable FluentColour findColour(@Nonnull String name) {
		Colour result = (Colour) findAttribute(name);
		if (result != null) {
			return new FluentColour(result);
		}
		return null;
	}
	/**
	 * Adds a metadata definition to this builder.
	 */
	public @Nonnull FluentDocument addContent(@Nonnull FluentContent content) {
		return addAttribute(content);
	}
	/**
	 * Finds a matching definition by key and returns null when no match exists.
	 */
	public @Nullable FluentContent findContent(@Nonnull String name) {
		Content result = (Content) findAttribute(name);
		if (result != null) {
			return new FluentContent(result);
		}
		return null;
	}
	/**
	 * Adds a metadata definition to this builder.
	 */
	public @Nonnull FluentDocument addImage(@Nonnull FluentImage image) {
		return addAttribute(image);
	}
	/**
	 * Finds a matching definition by key and returns null when no match exists.
	 */
	public @Nullable FluentImage findImage(@Nonnull String name) {
		Image result = (Image) findAttribute(name);
		if (result != null) {
			return new FluentImage(result);
		}
		return null;
	}
	/**
	 * Adds a metadata definition to this builder.
	 */
	public @Nonnull FluentDocument addGeometry(@Nonnull FluentGeometry geometry) {
		return addAttribute(geometry);
	}
	/**
	 * Finds a matching definition by key and returns null when no match exists.
	 */
	public @Nullable FluentGeometry findGeometry(@Nonnull String name) {
		Geometry result = (Geometry) findAttribute(name);
		if (result != null) {
			return new FluentGeometry(result);
		}
		return null;
	}
	/**
	 * Adds a metadata definition to this builder.
	 */
	public @Nonnull FluentDocument addId(@Nonnull FluentId id) {
		return addAttribute(id);
	}
	/**
	 * Finds a matching definition by key and returns null when no match exists.
	 */
	public @Nullable FluentId findId(@Nonnull String name) {
		Id result = (Id) findAttribute(name);
		if (result != null) {
			return new FluentId(result);
		}
		return null;
	}
	/**
	 * Adds a metadata definition to this builder.
	 */
	public @Nonnull FluentDocument addAssociation(@Nonnull FluentAssociation association) {
		return addAttribute(association);
	}
	/**
	 * Finds a matching definition by key and returns null when no match exists.
	 */
	public @Nullable FluentAssociation findAssociation(@Nonnull String name) {
		AssociationImpl result = (AssociationImpl) findAttribute(name);
		if (result != null) {
			return new FluentAssociation(result);
		}
		return null;
	}
	/**
	 * Adds a metadata definition to this builder.
	 */
	public @Nonnull FluentDocument addCollection(@Nonnull FluentCollection collection) {
		return addAttribute(collection);
	}
	/**
	 * Finds a matching definition by key and returns null when no match exists.
	 */
	public @Nullable FluentCollection findCollection(@Nonnull String name) {
		CollectionImpl result = (CollectionImpl) findAttribute(name);
		if (result != null) {
			return new FluentCollection(result);
		}
		return null;
	}
	/**
	 * Adds a metadata definition to this builder.
	 */
	public @Nonnull FluentDocument addInverseOne(@Nonnull FluentInverseOne inverseOne) {
		return addAttribute(inverseOne);
	}
	/**
	 * Finds a matching definition by key and returns null when no match exists.
	 */
	public @Nullable FluentInverseOne findInverseOne(@Nonnull String name) {
		InverseOne result = (InverseOne) findAttribute(name);
		if (result != null) {
			return new FluentInverseOne(result);
		}
		return null;
	}
	/**
	 * Adds a metadata definition to this builder.
	 */
	public @Nonnull FluentDocument addInverseMany(@Nonnull FluentInverseMany inverseMany) {
		return addAttribute(inverseMany);
	}
	/**
	 * Finds a matching definition by key and returns null when no match exists.
	 */
	public @Nullable FluentInverseMany findInverseMany(@Nonnull String name) {
		InverseMany result = (InverseMany) findAttribute(name);
		if (result != null) {
			return new FluentInverseMany(result);
		}
		return null;
	}

	/**
	 * Removes all attributes whose name matches {@code name}.
	 */
	public @Nonnull FluentDocument removeAttribute(@Nonnull String name) {
		document.getAttributes().removeIf(a -> name.equals(a.getName()));
		return this;
	}

	/**
	 * Removes all configured attributes.
	 */
	public @Nonnull FluentDocument clearAttributes() {
		document.getAttributes().clear();
		return this;
	}

	/**
	 * Appends a condition definition.
	 */
	public @Nonnull FluentDocument addCondition(@Nonnull FluentCondition condition) {
		document.getConditions().add(condition.get());
		return this;
	}

	/**
	 * Removes conditions that match {@code name}.
	 */
	public @Nonnull FluentDocument removeCondition(@Nonnull String name) {
		document.getConditions().removeIf(c -> name.equals(c.getName()));
		return this;
	}

	/**
	 * Removes all configured conditions.
	 */
	public @Nonnull FluentDocument clearConditions() {
		document.getConditions().clear();
		return this;
	}
	
	/**
	 * Finds a condition by name.
	 *
	 * @return a fluent wrapper around the first matching condition, or {@code null} when absent
	 */
	public @Nullable FluentCondition findCondition(@Nonnull String name) {
		ConditionMetaData result = document.getConditions().stream().filter(c -> name.equals(c.getName())).findAny().orElse(null);
		if (result != null) {
			return new FluentCondition(result);
		}
		return null;
	}
	/**
	 * Appends a document-level unique-constraint definition.
	 */
	public @Nonnull FluentDocument addUniqueConstraint(@Nonnull FluentDocumentUniqueConstraint constraint) {
		document.getUniqueConstraints().add(constraint.get());
		return this;
	}

	/**
	 * Removes document-level unique constraints that match {@code name}.
	 */
	public @Nonnull FluentDocument removeUniqueConstraint(@Nonnull String name) {
		document.getUniqueConstraints().removeIf(c -> name.equals(c.getName()));
		return this;
	}

	/**
	 * Removes all document-level unique constraints.
	 */
	public @Nonnull FluentDocument clearUniqueConstraint() {
		document.getUniqueConstraints().clear();
		return this;
	}

	/**
	 * Finds a document-level unique constraint by name.
	 *
	 * @return a fluent wrapper around the first matching constraint, or {@code null} when absent
	 */
	public @Nullable FluentDocumentUniqueConstraint findUniqueConstraint(@Nonnull String name) {
		org.skyve.impl.metadata.repository.document.UniqueConstraint result = document.getUniqueConstraints().stream().filter(c -> name.equals(c.getName())).findAny().orElse(null);
		if (result != null) {
			return new FluentDocumentUniqueConstraint(result);
		}
		return null;
	}
	
	/**
	 * Adds or replaces a custom property on the wrapped metadata.
	 * Properties keep the order in which they were first added.
	 *
	 * @param k the property name
	 * @param v the property value
	 * @return this builder
	 */
	public @Nonnull FluentDocument putProperty(@Nonnull String k, @Nonnull String v) {
		document.getProperties().put(k, v);
		return this;
	}

	/**
	 * Returns the mutable document metadata instance being built.
	 */
	public @Nonnull DocumentMetaData get() {
		return document;
	}
}
