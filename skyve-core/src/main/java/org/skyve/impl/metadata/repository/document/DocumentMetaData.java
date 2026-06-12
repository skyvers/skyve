package org.skyve.impl.metadata.repository.document;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

import org.skyve.domain.types.Decimal;
import org.skyve.domain.types.converters.Converter;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.domain.types.jaxb.CDATAAdapter;
import org.skyve.impl.generate.DomainGenerator;
import org.skyve.impl.metadata.model.AbstractAttribute;
import org.skyve.impl.metadata.model.InterfaceImpl;
import org.skyve.impl.metadata.model.document.AssociationImpl;
import org.skyve.impl.metadata.model.document.CollectionImpl;
import org.skyve.impl.metadata.model.document.ConditionImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.model.document.InverseMany;
import org.skyve.impl.metadata.model.document.InverseOne;
import org.skyve.impl.metadata.model.document.UniqueConstraintImpl;
import org.skyve.impl.metadata.model.document.field.Boolean;
import org.skyve.impl.metadata.model.document.field.Colour;
import org.skyve.impl.metadata.model.document.field.Content;
import org.skyve.impl.metadata.model.document.field.ConvertibleField;
import org.skyve.impl.metadata.model.document.field.Date;
import org.skyve.impl.metadata.model.document.field.DateTime;
import org.skyve.impl.metadata.model.document.field.Decimal10;
import org.skyve.impl.metadata.model.document.field.Decimal2;
import org.skyve.impl.metadata.model.document.field.Decimal5;
import org.skyve.impl.metadata.model.document.field.Enumeration;
import org.skyve.impl.metadata.model.document.field.Enumeration.EnumeratedValue;
import org.skyve.impl.metadata.model.document.field.Field;
import org.skyve.impl.metadata.model.document.field.Field.IndexType;
import org.skyve.impl.metadata.model.document.field.Geometry;
import org.skyve.impl.metadata.model.document.field.Id;
import org.skyve.impl.metadata.model.document.field.Image;
import org.skyve.impl.metadata.model.document.field.Integer;
import org.skyve.impl.metadata.model.document.field.LengthField;
import org.skyve.impl.metadata.model.document.field.LongInteger;
import org.skyve.impl.metadata.model.document.field.Markup;
import org.skyve.impl.metadata.model.document.field.Memo;
import org.skyve.impl.metadata.model.document.field.Text;
import org.skyve.impl.metadata.model.document.field.Time;
import org.skyve.impl.metadata.model.document.field.Timestamp;
import org.skyve.impl.metadata.model.document.field.validator.DateValidator;
import org.skyve.impl.metadata.model.document.field.validator.DecimalValidator;
import org.skyve.impl.metadata.model.document.field.validator.IntegerValidator;
import org.skyve.impl.metadata.model.document.field.validator.LongValidator;
import org.skyve.impl.metadata.model.document.field.validator.TextValidator;
import org.skyve.impl.metadata.model.document.field.validator.TextValidator.ValidatorType;
import org.skyve.impl.metadata.repository.ConvertibleMetaData;
import org.skyve.impl.metadata.repository.NamedMetaData;
import org.skyve.impl.metadata.repository.PropertyMapAdapter;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.ConverterName;
import org.skyve.metadata.DecoratedMetaData;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.Ordering;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.Attribute.UsageType;
import org.skyve.metadata.model.Dynamic;
import org.skyve.metadata.model.Extends;
import org.skyve.metadata.model.Persistent;
import org.skyve.metadata.model.document.Collection.CollectionType;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.DomainType;
import org.skyve.metadata.model.document.Interface;
import org.skyve.metadata.model.document.Relation;
import org.skyve.util.Icons;

import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlElementRef;
import jakarta.xml.bind.annotation.XmlElementRefs;
import jakarta.xml.bind.annotation.XmlElementWrapper;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

@XmlRootElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE, name = "document")
@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE,
			name = "document",
			propOrder = {"documentation",
							"extends",
							"abstract",
							"persistent",
							"dynamic",
							"singularAlias",
							"pluralAlias",
							"audited",
							"iconStyleClass",
							"icon16x16RelativeFilePath",
							"icon32x32RelativeFilePath",
							"description",
							"parentDocument",
							"bizKey",
							"implements",
							"attributes",
							"conditions",
							"uniqueConstraints",
							"properties"})
/**
 * JAXB root element for a document XML descriptor ({@code document.xml}), converted
 * to a runtime {@link Document} during repository bootstrap.
 *
 * <p>Holds the full structural definition of a Skyve document: class name,
 * persistence mapping, fields, associations, collections, conditions, unique
 * constraints, and actions.  Extends {@link NamedMetaData} to bind the document
 * to its symbolic name.
 *
 * <p>Threading: not thread-safe.  Instances are populated during JAXB unmarshalling
 * and are read-only once converted and placed in the repository cache.
 *
 * @see Document
 * @see org.skyve.impl.metadata.model.document.DocumentImpl
 */
public class DocumentMetaData extends NamedMetaData implements ConvertibleMetaData<Document>, DecoratedMetaData {
	private static final long serialVersionUID = 222166383815547958L;
	
	private static final String ATTRIBUTE_NAMED = " : The attribute named ";
	private static final String ENUMERATION_PREFIX = " : Enumeration ";
	private static final String FOR_VALIDATOR_ON_FIELD = " for validator on field ";
	private static final String IS_NOT_COERCIBLE_TO_TYPE = " is not coercible to type ";
	private static final String MAX_VALUE_OF = " : The max value of ";
	private static final String MIN_VALUE_OF = " : The min value of ";
	private static final String PRECISION_FOR_VALIDATOR_ON_FIELD = " : Precision for validator on field ";
	private static final String VALIDATOR_MUST_DEFINE_MIN_OR_MAX = " : The validator on field ";
	private static final String MUST_DEFINE_EITHER_MIN_OR_MAX = " must define either a min or max.";

	private static boolean isAsciiJavaIdentifierPath(String value) {
		boolean expectingIdentifierStart = true;
		boolean foundIdentifierPart = false;
		for (int i = 0, l = value.length(); i < l; i++) {
			char character = value.charAt(i);
			if (Character.isWhitespace(character)) {
				continue;
			}
			if (expectingIdentifierStart) {
				if (! isAsciiJavaIdentifierStart(character)) {
					return false;
				}
				expectingIdentifierStart = false;
				foundIdentifierPart = true;
			}
			else if (character == '.') {
				expectingIdentifierStart = true;
			}
			else if (! isAsciiJavaIdentifierPart(character)) {
				return false;
			}
		}
		return foundIdentifierPart && (! expectingIdentifierStart);
	}

	private static boolean isAsciiJavaIdentifierStart(char character) {
		return (character == '_') ||
				(character == '$') ||
				((character >= 'a') && (character <= 'z')) ||
				((character >= 'A') && (character <= 'Z'));
	}

	private static boolean isAsciiJavaIdentifierPart(char character) {
		return isAsciiJavaIdentifierStart(character) ||
				((character >= '0') && (character <= '9'));
	}

	private Extends inherits;
	private java.lang.Boolean abstractClass;
	private Persistent persistent;
	private Dynamic dynamic;
	private String singularAlias;
	private String pluralAlias;
	private String iconStyleClass;
	private String icon16x16RelativeFilePath;
	private String icon32x32RelativeFilePath;
	private java.lang.Boolean audited;
	private String description;
	private ParentDocument parentDocument;
	private BizKey bizKey;
	private List<Interface> interfaces = new ArrayList<>();
	private List<Attribute> attributes = new ArrayList<>();
	private List<ConditionMetaData> conditions = new ArrayList<>();
	private List<UniqueConstraint> uniqueConstraints = new ArrayList<>();
	private String documentation;
	private long lastModifiedMillis = Long.MAX_VALUE;

	@XmlElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
	@XmlJavaTypeAdapter(PropertyMapAdapter.class)
	private Map<String, String> properties = new TreeMap<>();

	/**
	 * Returns the document inheritance mode.
	 *
	 * @return inheritance strategy, or {@code null}
	 */
	public Extends getExtends() {
		return inherits;
	}

	/**
	 * Sets the document inheritance mode.
	 *
	 * @param inherits inheritance strategy
	 */
	@XmlElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
	public void setExtends(Extends inherits) {
		this.inherits = inherits;
	}

	/**
	 * Returns whether this document is abstract.
	 *
	 * @return {@code true} when abstract, otherwise {@code false} or {@code null}
	 */
	public java.lang.Boolean getAbstract() {
		return abstractClass;
	}

	/**
	 * Sets whether this document is abstract.
	 *
	 * @param abstractClass abstract flag
	 */
	@XmlElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
	public void setAbstract(java.lang.Boolean abstractClass) {
		this.abstractClass = abstractClass;
	}

	/**
	 * Returns persistence mapping metadata for this document.
	 *
	 * @return persistence metadata, or {@code null} for transient documents
	 */
	public Persistent getPersistent() {
		return persistent;
	}

	/**
	 * Sets persistence mapping metadata for this document.
	 *
	 * @param persistent persistence metadata
	 */
	@XmlElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE, required = false)
	public void setPersistent(Persistent persistent) {
		this.persistent = persistent;
	}

	/**
	 * Returns dynamic-behaviour metadata for this document.
	 *
	 * @return dynamic metadata, or {@code null}
	 */
	public Dynamic getDynamic() {
		return dynamic;
	}

	/**
	 * Sets dynamic-behaviour metadata for this document.
	 *
	 * @param dynamic dynamic metadata
	 */
	@XmlElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
	public void setDynamic(Dynamic dynamic) {
		this.dynamic = dynamic;
	}

	/**
	 * Returns the singular display alias.
	 *
	 * @return singular alias
	 */
	public String getSingularAlias() {
		return singularAlias;
	}

	/**
	 * Sets the singular display alias.
	 *
	 * @param singularAlias singular alias value
	 */
	@XmlElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE, required = true)
	public void setSingularAlias(String singularAlias) {
		this.singularAlias = UtilImpl.processStringValue(singularAlias);
	}

	/**
	 * Returns the plural display alias.
	 *
	 * @return plural alias
	 */
	public String getPluralAlias() {
		return pluralAlias;
	}

	/**
	 * Sets the plural display alias.
	 *
	 * @param pluralAlias plural alias value
	 */
	@XmlElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE, required = true)
	public void setPluralAlias(String pluralAlias) {
		this.pluralAlias = UtilImpl.processStringValue(pluralAlias);
	}

	/**
	 * Returns the relative path for the 16x16 document icon.
	 *
	 * @return 16x16 icon path, or {@code null}
	 */
	public String getIcon16x16RelativeFilePath() {
		return icon16x16RelativeFilePath;
	}

	/**
	 * Sets the relative path for the 16x16 document icon.
	 *
	 * @param icon16x16RelativeFilePath 16x16 icon path
	 */
	@XmlElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
	public void setIcon16x16RelativeFilePath(String icon16x16RelativeFilePath) {
		this.icon16x16RelativeFilePath = UtilImpl.processStringValue(icon16x16RelativeFilePath);
	}

	/**
	 * Returns the icon CSS class.
	 *
	 * @return icon style class, or {@code null}
	 */
	public String getIconStyleClass() {
		return iconStyleClass;
	}

	/**
	 * Sets the icon CSS class.
	 *
	 * @param iconStyleClass icon style class
	 */
	@XmlElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
	public void setIconStyleClass(String iconStyleClass) {
		this.iconStyleClass = UtilImpl.processStringValue(iconStyleClass);
	}

	/**
	 * Returns the relative path for the 32x32 document icon.
	 *
	 * @return 32x32 icon path, or {@code null}
	 */
	public String getIcon32x32RelativeFilePath() {
		return icon32x32RelativeFilePath;
	}

	/**
	 * Sets the relative path for the 32x32 document icon.
	 *
	 * @param icon32x32RelativeFilePath 32x32 icon path
	 */
	@XmlElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
	public void setIcon32x32RelativeFilePath(String icon32x32RelativeFilePath) {
		this.icon32x32RelativeFilePath = UtilImpl.processStringValue(icon32x32RelativeFilePath);
	}

	/**
	 * Returns whether auditing is enabled.
	 *
	 * @return audit flag, or {@code null} to use defaults
	 */
	public java.lang.Boolean getAudited() {
		return audited;
	}

	/**
	 * Sets whether auditing is enabled.
	 *
	 * @param audited audit flag
	 */
	@XmlElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
	public void setAudited(java.lang.Boolean audited) {
		this.audited = audited;
	}

	/**
	 * Returns the human-readable document description.
	 *
	 * @return document description, or {@code null}
	 */
	public String getDescription() {
		return description;
	}

	/**
	 * Sets the human-readable document description.
	 *
	 * @param description document description
	 */
	@XmlElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
	@XmlJavaTypeAdapter(CDATAAdapter.class)
	public void setDescription(String description) {
		this.description = UtilImpl.processStringValue(description);
	}

	/**
	 * Returns parent-document linkage metadata.
	 *
	 * @return parent document metadata, or {@code null}
	 */
	public ParentDocument getParentDocument() {
		return parentDocument;
	}

	/**
	 * Sets parent-document linkage metadata.
	 *
	 * @param parentDocument parent document metadata
	 */
	@XmlElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
	public void setParentDocument(ParentDocument parentDocument) {
		this.parentDocument = parentDocument;
	}

	/**
	 * Returns business-key metadata for this document.
	 *
	 * @return business-key metadata, or {@code null}
	 */
	public BizKey getBizKey() {
		return bizKey;
	}

	/**
	 * Sets business-key metadata for this document.
	 *
	 * @param bizKey business-key metadata
	 */
	@XmlElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
	public void setBizKey(BizKey bizKey) {
		this.bizKey = bizKey;
	}

	/**
	 * Returns implemented interfaces declared for this document.
	 *
	 * @return mutable list of interface declarations
	 */
	@XmlElementWrapper(namespace = XMLMetaData.DOCUMENT_NAMESPACE, name = "implements")
	@XmlElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE,
			name = "interface",
			type = InterfaceImpl.class)
	public List<Interface> getImplements() {
		return interfaces;
	}

	/**
	 * Returns declared document attributes in definition order.
	 *
	 * Keep this in sync with ViewModelMetaData
	 *
	 * @return mutable list of document attributes
	 */
	@XmlElementWrapper(namespace = XMLMetaData.DOCUMENT_NAMESPACE, name = "attributes", required = true)
	@XmlElementRefs({@XmlElementRef(type = Text.class),
						@XmlElementRef(type = Date.class),
						@XmlElementRef(type = Time.class),
						@XmlElementRef(type = DateTime.class),
						@XmlElementRef(type = Timestamp.class),
						@XmlElementRef(type = org.skyve.impl.metadata.model.document.field.Integer.class),
						@XmlElementRef(type = LongInteger.class),
						@XmlElementRef(type = Decimal2.class),
						@XmlElementRef(type = Decimal5.class),
						@XmlElementRef(type = Decimal10.class),
						@XmlElementRef(type = Boolean.class),
						@XmlElementRef(type = Enumeration.class),
						@XmlElementRef(type = Memo.class),
						@XmlElementRef(type = Markup.class),
						@XmlElementRef(type = Colour.class),
						@XmlElementRef(type = Content.class),
						@XmlElementRef(type = Image.class),
						@XmlElementRef(type = AssociationImpl.class),
						@XmlElementRef(type = CollectionImpl.class),
						@XmlElementRef(type = InverseOne.class),
						@XmlElementRef(type = InverseMany.class),
						@XmlElementRef(type = Geometry.class),
						@XmlElementRef(type = Id.class)})
	public List<Attribute> getAttributes() {
		return attributes;
	}

	/**
	 * Returns declared document conditions.
	 *
	 * @return mutable list of condition metadata
	 */
	@XmlElementWrapper(namespace = XMLMetaData.DOCUMENT_NAMESPACE, name = "conditions")
	@XmlElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE, name = "condition", required = true)
	public List<ConditionMetaData> getConditions() {
		return conditions;
	}

	/**
	 * Returns declared unique constraints.
	 *
	 * @return mutable list of unique constraint metadata
	 */
	@XmlElementWrapper(namespace = XMLMetaData.DOCUMENT_NAMESPACE, name = "uniqueConstraints")
	@XmlElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE, name = "constraint", required = false)
	public List<UniqueConstraint> getUniqueConstraints() {
		return uniqueConstraints;
	}

	/**
	 * Returns long-form document documentation.
	 *
	 * @return documentation text, or {@code null}
	 */
	public String getDocumentation() {
		return documentation;
	}

	/**
	 * Sets long-form document documentation.
	 *
	 * @param documentation documentation text
	 */
	@XmlElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
	@XmlJavaTypeAdapter(CDATAAdapter.class)
	public void setDocumentation(String documentation) {
		this.documentation = UtilImpl.processStringValue(documentation);
	}

	/**
	 * Returns decorator properties defined on this document descriptor.
	 *
	 * @return mutable document property map
	 */
	@Override
	public Map<String, String> getProperties() {
		return properties;
	}

	/**
	 * Returns the source metadata last-modified timestamp used for reload checks.
	 *
	 * @return source metadata last-modified timestamp in milliseconds
	 */
	@Override
	public long getLastModifiedMillis() {
		return lastModifiedMillis;
	}

	/**
	 * Sets the source metadata last-modified timestamp used for reload checks.
	 *
	 * @param lastModifiedMillis source metadata last-modified timestamp in milliseconds
	 */
	@XmlTransient
	public void setLastModifiedMillis(long lastModifiedMillis) {
		this.lastModifiedMillis = lastModifiedMillis;
	}

	/**
	 * Converts this JAXB document descriptor into runtime document metadata.
	 *
	 * <p>Validates required aliases, biz-key rules, dynamic mappings, attribute
	 * names, converter compatibility, uniqueness constraints, and derived defaults
	 * before returning a populated {@link DocumentImpl}.
	 *
	 * @param metaDataName metadata path used in validation error messages
	 * @return the populated runtime document metadata
	 * @throws MetaDataException if the descriptor is incomplete or internally inconsistent
	 */
	@Override
	@SuppressWarnings({"java:S3776", "java:S6541"}) // complexity OK
	public Document convert(String metaDataName) {
		DocumentImpl result = new DocumentImpl();
		result.setLastModifiedMillis(getLastModifiedMillis());

		// Set document metadata
		String value = getName();
		if (value == null) {
			throw new MetaDataException(metaDataName + " : The document [name] is required");
		}
		result.setName(value);
		result.setExtends(getExtends());
		result.setAbstract(java.lang.Boolean.TRUE.equals(getAbstract()));
		result.setPersistent(getPersistent());
		Dynamic resultDynamic = getDynamic();
		result.setDynamism(resultDynamic);
		value = getSingularAlias();
		if (value == null) {
			throw new MetaDataException(metaDataName + " : The document [singularAlias] is required");
		}
		result.setSingularAlias(value);
		value = getPluralAlias();
		if (value == null) {
			throw new MetaDataException(metaDataName + " : The document [pluralAlias] is required");
		}
		result.setPluralAlias(value);

		String icon16 = getIcon16x16RelativeFilePath();
		String icon32 = getIcon32x32RelativeFilePath();
		String icon = getIconStyleClass();

		if ((icon16 == null) && (icon32 == null) && (icon == null)) {
			icon = Icons.FONT_DOCUMENT;
		}
		else {
			if (icon16 == null) {
				icon16 = icon32;
			}
			if (icon32 == null) {
				icon32 = icon16;
			}
		}

		result.setIcon16x16RelativeFileName(icon16);
		result.setIcon32x32RelativeFileName(icon32);
		result.setIconStyleClass(icon);

		// audited defaults to true when not present
		result.setAudited(! java.lang.Boolean.FALSE.equals(getAudited()));

		result.setDescription(getDescription());

		ParentDocument parent = getParentDocument();
		if (parent != null) {
			result.setParentDocumentName(parent.getParentDocumentName());
			result.setParentDatabaseIndex(parent.getDatabaseIndex());
		}

		String bizKeyExpression = null;
		String bizKeyCode = null;
		if (bizKey != null) {
			bizKeyExpression = bizKey.getExpression();
			bizKeyCode = bizKey.getCode();
			result.setBizKeySensitity(bizKey.getSensitivity());
		}

		Persistent resultPersistent = result.getPersistent();
		if (resultPersistent != null) {
			if (bizKey == null) {
				throw new MetaDataException(metaDataName + " : The document [bizKey] is required for a persistent or mapped document");
			}
			if ((bizKeyCode == null) && (bizKeyExpression == null)) {
				throw new MetaDataException(metaDataName + " : The document [bizKey] requires either some code or an expression defined.");
			}
		}

		if (resultDynamic != null) {
			resultDynamic.getActions().forEach((k, v) -> {
				if (UtilImpl.processStringValue(k) == null) {
					throw new MetaDataException(metaDataName + " : The attribute [name] in dynamic actions is required");
				}
				if (UtilImpl.processStringValue(v) == null) {
					throw new MetaDataException(metaDataName + " : The attribute [className] in dynamic actions is required");
				}
			});

			resultDynamic.getImages().forEach((k, v) -> {
				if (UtilImpl.processStringValue(k) == null) {
					throw new MetaDataException(metaDataName + " : The attribute [name] in dynamic images is required");
				}
				if (UtilImpl.processStringValue(v) == null) {
					throw new MetaDataException(metaDataName + " : The attribute [className] in dynamic images is required");
				}
			});

			resultDynamic.getModels().forEach((k, v) -> {
				if (UtilImpl.processStringValue(k) == null) {
					throw new MetaDataException(metaDataName + " : The attribute [name] in dynamic models is required");
				}
				if (UtilImpl.processStringValue(v) == null) {
					throw new MetaDataException(metaDataName + " : The attribute [className] in dynamic models is required");
				}
			});
		}

		if (bizKeyCode != null) {
			result.setBizKeyMethodCode(bizKeyCode);
		}
		else if (bizKeyExpression != null) {
			StringBuilder sb = new StringBuilder(128);
			sb.append("\t\ttry {\n");
			sb.append("\t\t\treturn org.skyve.util.Binder.formatMessage(\"").append(bizKeyExpression).append("\", this);\n");
			sb.append("\t\t}\n");
			sb.append("\t\tcatch (@SuppressWarnings(\"unused\") Exception e) {\n");
			sb.append("\t\t\treturn \"Unknown\";\n");
			sb.append("\t\t}");
			result.setBizKeyMethodCode(sb.toString());
			result.setBizKeyExpression(bizKeyExpression);
		}
		else {
			result.setBizKeyMethodCode("\t\treturn toString();\n");
		}

		if ((resultPersistent == null) && java.lang.Boolean.TRUE.equals(result.getParentDatabaseIndex())) {
			throw new MetaDataException(metaDataName + " : [parentDocument.index] CANNOT be true for a transient document");
		}
		Set<String> attributeNames = new TreeSet<>();

		// Set interfaces metadata
		if (interfaces != null) {
			for (Interface interfaceMetaData : interfaces) {
				String interfaceName = interfaceMetaData.getInterfaceName();
				if (interfaceName == null || interfaceName.isEmpty()) {
					throw new MetaDataException(metaDataName + " : Fully qualified interface name is required.");
				}

				final InterfaceImpl interfaceImpl = new InterfaceImpl();
				interfaceImpl.setInterfaceName(interfaceName);

				result.putInterface(interfaceImpl);
			}
		}

		// Set attribute metadata
		if (attributes != null) {
			for (Attribute attribute : getAttributes()) {
				value = attribute.getName();
				if (value == null) {
					throw new MetaDataException(metaDataName + " : The attribute [name] is required");
				}
				if (BindUtil.isImplicit(value)) {
					throw new MetaDataException(metaDataName + ATTRIBUTE_NAMED + value + " is already an implicit attribute.");
				}
				if (! value.equals(BindUtil.toJavaInstanceIdentifier(value))) {
					throw new MetaDataException(metaDataName + ATTRIBUTE_NAMED + value + " is not a valid attribute name. This should be camel case with no punctuation");
				}
				// do not allow unicode document names, see https://hibernate.atlassian.net/browse/HHH-13383
				if (! isAsciiJavaIdentifierPath(value)) {
					throw new MetaDataException(metaDataName + ATTRIBUTE_NAMED + value + " must only contain non-unicode letters and digits.");
				}
				// not required but has a required message
				if ((! attribute.isRequired()) && attribute.getRequiredMessage() != null) {
					throw new MetaDataException(metaDataName + ATTRIBUTE_NAMED + value + " is not required but has a requiredMessage.");
				}
				if (! attributeNames.add(value)) {
					throw new MetaDataException(metaDataName + " : Duplicate attribute named " + value);
				}

				value = attribute.getDisplayName();
				if (value == null) {
					throw new MetaDataException(metaDataName + " : The attribute [displayName] is required for attribute " + attribute.getName());
				}

				if (DomainGenerator.JAVA_RESERVED_WORDS.contains(attribute.getName().toLowerCase())) {
					throw new MetaDataException(String.format("%s : %s is a reserved word and cannot be used as an attribute name.",
							metaDataName, attribute.getName()));
				}

				// Default auditing to off for view attributes
				// that don't have an audited value set in their definition.
				if ((attribute instanceof AbstractAttribute aa) &&
						(aa.getAuditedBool() == null) &&
						UsageType.view.equals(attribute.getUsage())) {
					((AbstractAttribute) attribute).setAudited(false);
				}

				AttributeType type = attribute.getAttributeType();
				if (attribute instanceof Field field) {
					Converter<?> converter = null;

					if ((AttributeType.memo.equals(type) || AttributeType.markup.equals(type)) &&
							(field.getIndex() == null)) {
						field.setIndex(IndexType.textual);
					}

					// Set persistent attribute false for non-persistent documents
					if (resultPersistent == null) {
						field.setPersistent(false);
					}

					if (resultDynamic != null) {
						field.setDynamic(true);
					}

					if ((field.getGenerated() != null) && (! field.isPersistent())) {
						throw new MetaDataException(metaDataName + " : The [generated] element cannot be defined on non-persistent field " + field.getName());
					}

					if (attribute instanceof ConvertibleField convertibleField) {
						ConverterName converterName = convertibleField.getConverterName();
						if (converterName != null) {
							converter = converterName.getConverter();
							if (! type.equals(converter.getAttributeType())) {
								throw new MetaDataException(metaDataName + " : The converter " + converterName + " of field " + attribute.getName() + " is not a converter for type " + type);
							}
							convertibleField.setConverter(converter);
						}
					}

					// Do not load the enum class to get the implementing type here as the enum could be a reference
					// to an enum in the same document or a cyclic reference across multiple documents that are not loaded yet.
					// This call is made whilst things are in flux and it would result in infinite recursion.
					Class<?> implementingType = (attribute instanceof Enumeration) ? Enum.class : attribute.getImplementingType();

					// NB Default values & bizKey expressions are checked in LocalDesignRepository where we have access to the document

					DateValidator dateValidator = null;
					DecimalValidator decimalValidator = null;
					IntegerValidator integerValidator = null;
					LongValidator longValidator = null;

					if (field instanceof Text text) {
						TextValidator validator = text.getValidator();
						if (validator != null) {
							String regex = validator.getRegularExpression();
							ValidatorType validatorType = validator.getType();
							if ((regex == null) && (validatorType == null)) {
								throw new MetaDataException(metaDataName + " : A regular expression and/or a validator type is required for validator on field " + field.getName());
							}
						}
					}
					else if (field instanceof Date date) {
						dateValidator = date.getValidator();
					}
					else if (field instanceof DateTime dateTime) {
						dateValidator = dateTime.getValidator();
					}
					else if (field instanceof Time time) {
						dateValidator = time.getValidator();
					}
					else if (field instanceof Timestamp timestamp) {
						dateValidator = timestamp.getValidator();
					}
					else if (field instanceof Decimal2 decimal) {
						decimalValidator = decimal.getValidator();
						if (decimalValidator != null) {
							java.lang.Integer precision = decimalValidator.getPrecision();
							if ((precision != null) && (precision.intValue() > 2)) {
								throw new MetaDataException(metaDataName + PRECISION_FOR_VALIDATOR_ON_FIELD + field.getName() + " cannot be > 2");
							}
						}
					}
					else if (field instanceof Decimal5 decimal) {
						decimalValidator = decimal.getValidator();
						if (decimalValidator != null) {
							java.lang.Integer precision = decimalValidator.getPrecision();
							if ((precision != null) && (precision.intValue() > 5)) {
								throw new MetaDataException(metaDataName + PRECISION_FOR_VALIDATOR_ON_FIELD + field.getName() + " cannot be > 5");
							}
						}
					}
					else if (field instanceof Decimal10 decimal) {
						decimalValidator = decimal.getValidator();
						if (decimalValidator != null) {
							java.lang.Integer precision = decimalValidator.getPrecision();
							if ((precision != null) && (precision.intValue() > 10)) {
								throw new MetaDataException(metaDataName + PRECISION_FOR_VALIDATOR_ON_FIELD + field.getName() + " cannot be > 10");
							}
						}
					}
					else if (field instanceof Integer integer) {
						integerValidator = integer.getValidator();
					}
					else if (field instanceof LongInteger integer) {
						longValidator = integer.getValidator();
					}

					if (dateValidator != null) {
						String xmlMin = dateValidator.getXmlMin();
						String xmlMax = dateValidator.getXmlMax();
						if ((xmlMin == null) && (xmlMax == null)) {
							throw new MetaDataException(metaDataName + VALIDATOR_MUST_DEFINE_MIN_OR_MAX + attribute.getName() + MUST_DEFINE_EITHER_MIN_OR_MAX);
						}
						if (xmlMin != null) {
							try {
								dateValidator.setMin((java.util.Date) BindUtil.fromSerialised(converter, implementingType, xmlMin));
							}
							catch (@SuppressWarnings("unused") Exception e) {
								throw new MetaDataException(metaDataName + MIN_VALUE_OF + xmlMin + FOR_VALIDATOR_ON_FIELD + field.getName() +
																IS_NOT_COERCIBLE_TO_TYPE + type +
																".  Values should be specified in the format of the field converter (if defined) or generic formats otherwise - date based types should be expressed as a standard XML date format - YYYY-MM-DD or YYYY-MM-DDTHH24:MM:SS");
							}
						}
						if (xmlMax != null) {
							try {
								dateValidator.setMax((java.util.Date) BindUtil.fromSerialised(converter, implementingType, xmlMax));
							}
							catch (@SuppressWarnings("unused") Exception e) {
								throw new MetaDataException(metaDataName + MAX_VALUE_OF + xmlMax + FOR_VALIDATOR_ON_FIELD + field.getName() +
																IS_NOT_COERCIBLE_TO_TYPE + type +
																".  Values should be specified in the format of the field converter (if defined) or generic formats otherwise - date based types should be expressed as a standard XML date format - YYYY-MM-DD or YYYY-MM-DDTHH24:MM:SS");
							}
						}
					}
					if (decimalValidator != null) {
						String xmlMin = decimalValidator.getXmlMin();
						String xmlMax = decimalValidator.getXmlMax();
						if ((xmlMin == null) && (xmlMax == null)) {
							throw new MetaDataException(metaDataName + VALIDATOR_MUST_DEFINE_MIN_OR_MAX + attribute.getName() + MUST_DEFINE_EITHER_MIN_OR_MAX);
						}
						if (xmlMin != null) {
							try {
								decimalValidator.setMin((Decimal) BindUtil.fromSerialised(converter, implementingType, xmlMin));
							}
							catch (@SuppressWarnings("unused") Exception e) {
								throw new MetaDataException(metaDataName + MIN_VALUE_OF + xmlMin + FOR_VALIDATOR_ON_FIELD + field.getName() +
																IS_NOT_COERCIBLE_TO_TYPE + type + ".  Values should be specified in the format of the field converter (if defined) or generic formats otherwise - decimal based types should be expressed as floating point expressions ie 1.1");
							}
						}
						if (xmlMax != null) {
							try {
								decimalValidator.setMax((Decimal) BindUtil.fromSerialised(converter, implementingType, xmlMax));
							}
							catch (@SuppressWarnings("unused") Exception e) {
								throw new MetaDataException(metaDataName + MAX_VALUE_OF + xmlMax + FOR_VALIDATOR_ON_FIELD + field.getName() +
										IS_NOT_COERCIBLE_TO_TYPE + type + ".  Values should be specified in the format of the field converter (if defined) or generic formats otherwise - decimal based types should be expressed as floating point expressions ie 1.1");
							}
						}
					}
					if (integerValidator != null) {
						String xmlMin = integerValidator.getXmlMin();
						String xmlMax = integerValidator.getXmlMax();
						if ((xmlMin == null) && (xmlMax == null)) {
							throw new MetaDataException(metaDataName + VALIDATOR_MUST_DEFINE_MIN_OR_MAX + attribute.getName() + MUST_DEFINE_EITHER_MIN_OR_MAX);
						}
						if (xmlMin != null) {
							try {
								integerValidator.setMin((java.lang.Integer) BindUtil.fromSerialised(converter, implementingType, xmlMin));
							}
							catch (@SuppressWarnings("unused") Exception e) {
								throw new MetaDataException(metaDataName + MIN_VALUE_OF + xmlMin + FOR_VALIDATOR_ON_FIELD + field.getName() +
																IS_NOT_COERCIBLE_TO_TYPE + type + ".  Values should be specified in the format of the field converter (if defined) or generic formats otherwise - integer based types should be expressed as integer expressions ie 1");
							}
						}
						if (xmlMax != null) {
							try {
								integerValidator.setMax((java.lang.Integer) BindUtil.fromSerialised(converter, implementingType, xmlMax));
							}
							catch (@SuppressWarnings("unused") Exception e) {
								throw new MetaDataException(metaDataName + MAX_VALUE_OF + xmlMax + FOR_VALIDATOR_ON_FIELD + field.getName() +
										IS_NOT_COERCIBLE_TO_TYPE + type + ".  Values should be specified in the format of the field converter (if defined) or generic formats otherwise - integer based types should be expressed as integer expressions ie 1");
							}
						}
					}
					if (longValidator != null) {
						String xmlMin = longValidator.getXmlMin();
						String xmlMax = longValidator.getXmlMax();
						if ((xmlMin == null) && (xmlMax == null)) {
							throw new MetaDataException(metaDataName + VALIDATOR_MUST_DEFINE_MIN_OR_MAX + attribute.getName() + MUST_DEFINE_EITHER_MIN_OR_MAX);
						}
						if (xmlMin != null) {
							try {
								longValidator.setMin((java.lang.Long) BindUtil.fromSerialised(converter, implementingType, xmlMin));
							}
							catch (@SuppressWarnings("unused") Exception e) {
								throw new MetaDataException(metaDataName + MIN_VALUE_OF + xmlMin + FOR_VALIDATOR_ON_FIELD + field.getName() +
																IS_NOT_COERCIBLE_TO_TYPE + type + ".  Values should be specified in the format of the field converter (if defined) or generic formats otherwise - long based types should be expressed as long expressions ie 1");
							}
						}
						if (xmlMax != null) {
							try {
								longValidator.setMax((java.lang.Long) BindUtil.fromSerialised(converter, implementingType, xmlMax));
							}
							catch (@SuppressWarnings("unused") Exception e) {
								throw new MetaDataException(metaDataName + MAX_VALUE_OF + xmlMax + FOR_VALIDATOR_ON_FIELD + field.getName() +
										IS_NOT_COERCIBLE_TO_TYPE + type + ".  Values should be specified in the format of the field converter (if defined) or generic formats otherwise - long based types should be expressed as long expressions ie 1");
							}
						}
					}

					if ((attribute instanceof LengthField lengthField) && (lengthField.getLength() < 1)) {
						throw new MetaDataException(metaDataName + " : The length of field " + attribute.getName() + " is not a valid length");
					}

					if (attribute instanceof Enumeration enumeration) {
						// Enumeration can be defined inline (ie a new one) or
						// a reference (module, document, attribute) to another definition or
						// as an external Enumeration (java enum) implementation class.
						String moduleRef = enumeration.getModuleRef();
						String documentRef = enumeration.getDocumentRef();
						String attributeRef = enumeration.getAttributeRef();
						List<EnumeratedValue> values = enumeration.getXmlValues();

						// Reference
						if ((moduleRef != null) || (documentRef != null) || (attributeRef != null)) { // reference
							if (attributeRef == null) {
								throw new MetaDataException(metaDataName + ENUMERATION_PREFIX + attribute.getName() +
																" is defined as a reference to another enum but [attributeRef] is not defined.");
							}
							if ((moduleRef != null) && (documentRef == null)) {
								throw new MetaDataException(metaDataName + ENUMERATION_PREFIX + attribute.getName() +
																" has a [moduleRef] but no documentRef.");
							}
							if (! values.isEmpty()) {
								throw new MetaDataException(metaDataName + ENUMERATION_PREFIX + attribute.getName() +
																" is defined as a reference to another enum but has [values] defined.");
							}
							if (enumeration.getXmlTypeName() != null) {
								throw new MetaDataException(metaDataName + ENUMERATION_PREFIX + attribute.getName() +
																" is defined as a reference to another enum but has [typeName] defined.");
							}
							if (enumeration.getXmlImplementingEnumClassName() != null) {
								throw new MetaDataException(metaDataName + ENUMERATION_PREFIX + attribute.getName() +
																" is defined as a reference to another enum but has [implementingEnumClassName] defined.");
							}
						}
						else {
							String implementingEnumClassName = enumeration.getXmlImplementingEnumClassName();
							if (implementingEnumClassName != null) { // implementing
								if (! values.isEmpty()) {
									throw new MetaDataException(metaDataName + ENUMERATION_PREFIX + attribute.getName() +
																	" is defined with an [implementingEnumClassName] but has [values] defined.");
								}
							}
							else { // definition
								if (values.isEmpty()) {
									throw new MetaDataException(metaDataName + ENUMERATION_PREFIX + attribute.getName() +
																	" has no [values] defined.");
								}
							}
						}
						enumeration.setOwningDocument(result);

						// check to see if there is an overridden domain type set,
						// otherwise set it to constant
						if (enumeration.getDomainType() == null) {
							enumeration.setDomainType(DomainType.constant);
						}
					}

					result.putAttribute(attribute);
				}
				else if (attribute instanceof Relation relation) {
					value = relation.getDocumentName();
					if (value == null) {
						throw new MetaDataException(metaDataName + " : The relation [documentName] is required for relation " + relation.getName());
					}

					if (relation instanceof AssociationImpl association) {
						// Set persistent attribute false for non-persistent documents
						if (resultPersistent == null) {
							association.setPersistent(false);
						}
						if (association.getType() == null) {
							throw new MetaDataException(metaDataName + " : The association [type] is required for association " +
															relation.getName());
						}
						if ((! association.isPersistent()) && (association.getDatabaseIndex() != null)) {
							throw new MetaDataException(metaDataName + " : The association [databaseIndex] is NOT required for transient association " +
															relation.getName());
						}
					}
					else if (relation instanceof CollectionImpl collection) {
						// Set persistent attribute false for non-persistent documents
						if (resultPersistent == null) {
							collection.setPersistent(false);
						}

						// Ordered and Ordering are mutually exclusive
						List<Ordering> orderings = collection.getOrdering();
						if (java.lang.Boolean.TRUE.equals(collection.getOrdered()) && (! orderings.isEmpty())) {
							throw new MetaDataException(metaDataName + " : The collection [ordered] and [orderings] are mutually exclusive for collection " +
															relation.getName());
						}

						java.lang.Boolean ownerDatabaseIndex = collection.getOwnerDatabaseIndex();
						java.lang.Boolean elementDatabaseIndex = collection.getElementDatabaseIndex();

						// Check for compound bindings.
						// Hibernate defines collection sort order in terms of SQL columns in the ORM.
						// This means, no compound bindings and references must actually be to foreign key columns.
						// We need to indicate to the framework that we will need to order the collection ourselves in memory.
						if (collection.isPersistent()) {
							for (Ordering ordering : orderings) {
								String by = ordering.getBy();
								if (by.indexOf('.') >= 0) {
									collection.setComplexOrdering(true);
									break;
								}
							}
						}
						else {
							if (ownerDatabaseIndex != null) {
								throw new MetaDataException(metaDataName + " : The collection [ownerDatabaseIndex] is NOT required for transient collection " +
																relation.getName());
							}
							if (elementDatabaseIndex != null) {
								throw new MetaDataException(metaDataName + " : The collection [elementDatabaseIndex] is NOT required for transient collection " +
																relation.getName());
							}
						}

						if (CollectionType.child.equals(collection.getType())) {
							if (ownerDatabaseIndex != null) {
								throw new MetaDataException(metaDataName + " : The collection [ownerDatabaseIndex] is NOT applicable to child collection " +
																relation.getName());
							}
							if (elementDatabaseIndex != null) {
								throw new MetaDataException(metaDataName + " : The collection [elementDatabaseIndex] is NOT applicable to child collection " +
																relation.getName());
							}
						}
					}
					else if (relation instanceof InverseMany inverse) {
						List<Ordering> orderings = inverse.getOrdering();
						for (Ordering ordering : orderings) {
							String by = ordering.getBy();
							if (by.indexOf('.') >= 0) {
								inverse.setComplexOrdering(true);
								break;
							}
						}
					}
					result.putRelation(relation);
				}
			}
		}

		// Set conditions
		if (conditions != null) {
			for (ConditionMetaData conditionMetaData : conditions) {
				String conditionName = conditionMetaData.getName();
				if (conditionName == null) {
					throw new MetaDataException(metaDataName + " : A condition [name] is required.");
				}
				// Check that conditions do not start with is or not and are a valid java bean property
				if (conditionName.startsWith("is")) {
					throw new MetaDataException(metaDataName + " Condition name " + conditionName + " cannot start with 'is' - the 'is' prefix is generated in the bean method.");
				}
				if (conditionName.startsWith("not")) {
					throw new MetaDataException(metaDataName + " : Condition name " + conditionName + " cannot start with 'not'.  The negated condition will be generated automatically.");
				}
				if (! conditionName.equals(BindUtil.toJavaInstanceIdentifier(conditionName))) {
					throw new MetaDataException(metaDataName + " : The condition named " + conditionName + " is not a valid condition name. This should be camel case with no punctuation.");
				}
				if (! attributeNames.add(conditionName)) {
					throw new MetaDataException(metaDataName + " : Condition name clashes with field/association/reference/condition named " + conditionName);
				}

				String conditionExpression = conditionMetaData.getExpression();
				if (conditionExpression == null) {
					throw new MetaDataException(metaDataName + " : A condition [expression] is required.");
				}

				ConditionImpl condition = new ConditionImpl();
				condition.setExpression(conditionExpression);
				condition.setDocumentation(conditionMetaData.getDocumentation());
				condition.setDescription(conditionMetaData.getDescription());
				condition.setUsage(conditionMetaData.getUsage());
				condition.getProperties().putAll(conditionMetaData.getProperties());

				if (result.getConditions().put(conditionName, condition) != null) {
					throw new MetaDataException(metaDataName + " : A duplicate condition of " + conditionName + " is defined.");
				}
			}
		}

		// Set unique constraints.
		if (uniqueConstraints != null) {
			Set<String> constraintNames = new TreeSet<>();

			for (UniqueConstraint constraintMetaData : uniqueConstraints) {
				if (persistent == null) {
					throw new MetaDataException(metaDataName + " : A non-persistent document cannot have unique constraints.");
				}
				if (persistent.isPolymorphicallyMapped()) {
					throw new MetaDataException(metaDataName + " : A mapped polymorphic document cannot have unique constraints. These must be defined on the extending persistent documents.");
				}
				UniqueConstraintImpl constraint = new UniqueConstraintImpl();
				value = constraintMetaData.getName();
				if (value == null) {
					throw new MetaDataException(metaDataName + " : The unique constraint [name] is required");
				}
				if (! constraintNames.add(value)) {
					throw new MetaDataException(metaDataName + " : Duplicate unique constraint named " + value);
				}
				constraint.setName(value);
				constraint.setScope(constraintMetaData.getScope());
				constraint.setDescription(constraintMetaData.getDescription());
				value = constraintMetaData.getMessage();
				if (value == null) {
					throw new MetaDataException(metaDataName + " : The unique constraint [message] is required for constraint " +
													constraint.getName());
				}
				constraint.setMessage(value);

				Set<String> refNames = new TreeSet<>();
				List<FieldReference> fieldReferences = constraintMetaData.getFieldReferences();
				if (fieldReferences != null) {
					for (FieldReference ref : fieldReferences) {
						value = ref.getRef();
						if (value == null) {
							throw new MetaDataException(metaDataName + " : The unique constraint field reference [ref] is required for constraint " +
															constraint.getName());
						}
						if ( !refNames.add(value)) {
							throw new MetaDataException(metaDataName + " : Duplicate unique constraint reference [ref] of " +
															value + " in constraint " + constraint.getName());
						}
						constraint.getFieldNames().add(value);
					}
				}

				constraint.getProperties().putAll(constraintMetaData.getProperties());

				result.putUniqueConstraint(constraint);
			}
		}

		result.setDocumentation(getDocumentation());
		result.getProperties().putAll(getProperties());

		return result;
	}
}
