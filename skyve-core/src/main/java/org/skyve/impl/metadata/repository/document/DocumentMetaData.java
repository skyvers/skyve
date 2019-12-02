package org.skyve.impl.metadata.repository.document;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElementRef;
import javax.xml.bind.annotation.XmlElementRefs;
import javax.xml.bind.annotation.XmlElementWrapper;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import org.apache.commons.lang3.StringUtils;
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
import org.skyve.impl.metadata.model.document.field.ConvertableField;
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
import org.skyve.impl.metadata.repository.AbstractRepository;
import org.skyve.impl.metadata.repository.NamedMetaData;
import org.skyve.impl.metadata.repository.PersistentMetaData;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.ConverterName;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.Attribute.UsageType;
import org.skyve.metadata.model.Extends;
import org.skyve.metadata.model.Persistent;
import org.skyve.metadata.model.document.Association;
import org.skyve.metadata.model.document.Collection;
import org.skyve.metadata.model.document.Collection.CollectionType;
import org.skyve.metadata.model.document.Collection.Ordering;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.DomainType;
import org.skyve.metadata.model.document.Interface;
import org.skyve.metadata.model.document.Relation;

@XmlRootElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE, name = "document")
@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE, 
			name = "document",
			propOrder = {"documentation",
							"extends",
							"abstract",
							"persistent",
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
							"uniqueConstraints"})
public class DocumentMetaData extends NamedMetaData implements PersistentMetaData<Document> {
	private static final long serialVersionUID = 222166383815547958L;

	private static final String DEFAULT_DOCUMENT_ICON_STYLE_CLASS = "fa fa-file-o";

	private Extends inherits;
	private java.lang.Boolean abstractClass;
	private Persistent persistent;
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

	public Extends getExtends() {
		return inherits;
	}

	@XmlElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
	public void setExtends(Extends inherits) {
		this.inherits = inherits;
	}

	public java.lang.Boolean getAbstract() {
		return abstractClass;
	}

	@XmlElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
	public void setAbstract(java.lang.Boolean abstractClass) {
		this.abstractClass = abstractClass;
	}

	public Persistent getPersistent() {
		return persistent;
	}

	@XmlElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE, required = false)
	public void setPersistent(Persistent persistent) {
		this.persistent = persistent;
	}

	public String getSingularAlias() {
		return singularAlias;
	}

	@XmlElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE, required = true)
	public void setSingularAlias(String singularAlias) {
		this.singularAlias = UtilImpl.processStringValue(singularAlias);
	}

	public String getPluralAlias() {
		return pluralAlias;
	}

	@XmlElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE, required = true)
	public void setPluralAlias(String pluralAlias) {
		this.pluralAlias = UtilImpl.processStringValue(pluralAlias);
	}

	public String getIcon16x16RelativeFilePath() {
		return icon16x16RelativeFilePath;
	}

	@XmlElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
	public void setIcon16x16RelativeFilePath(String icon16x16RelativeFilePath) {
		this.icon16x16RelativeFilePath = UtilImpl.processStringValue(icon16x16RelativeFilePath);
	}

	public String getIconStyleClass() {
		return iconStyleClass;
	}

	@XmlElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
	public void setIconStyleClass(String iconStyleClass) {
		this.iconStyleClass = UtilImpl.processStringValue(iconStyleClass);
	}

	public String getIcon32x32RelativeFilePath() {
		return icon32x32RelativeFilePath;
	}

	@XmlElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
	public void setIcon32x32RelativeFilePath(String icon32x32RelativeFilePath) {
		this.icon32x32RelativeFilePath = UtilImpl.processStringValue(icon32x32RelativeFilePath);
	}

	public java.lang.Boolean getAudited() {
		return audited;
	}

	@XmlElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
	public void setAudited(java.lang.Boolean audited) {
		this.audited = audited;
	}

	public String getDescription() {
		return description;
	}

	@XmlElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
	@XmlJavaTypeAdapter(CDATAAdapter.class)
	public void setDescription(String description) {
		this.description = UtilImpl.processStringValue(description);
	}

	public ParentDocument getParentDocument() {
		return parentDocument;
	}

	@XmlElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
	public void setParentDocument(ParentDocument parentDocument) {
		this.parentDocument = parentDocument;
	}

	public BizKey getBizKey() {
		return bizKey;
	}

	@XmlElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
	public void setBizKey(BizKey bizKey) {
		this.bizKey = bizKey;
	}

	@XmlElementWrapper(namespace = XMLMetaData.DOCUMENT_NAMESPACE, name = "implements")
	@XmlElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE,
			name = "interface",
			type = InterfaceImpl.class)
	public List<Interface> getImplements() {
		return interfaces;
	}

	// Keep this in sync with ViewModelMetaData
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

	@XmlElementWrapper(namespace = XMLMetaData.DOCUMENT_NAMESPACE, name = "conditions")
	@XmlElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE, name = "condition", required = true)
	public List<ConditionMetaData> getConditions() {
		return conditions;
	}

	@XmlElementWrapper(namespace = XMLMetaData.DOCUMENT_NAMESPACE, name = "uniqueConstraints")
	@XmlElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE, name = "constraint", required = false)
	public List<UniqueConstraint> getUniqueConstraints() {
		return uniqueConstraints;
	}

	public String getDocumentation() {
		return documentation;
	}

	@XmlElement(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
	@XmlJavaTypeAdapter(CDATAAdapter.class)
	public void setDocumentation(String documentation) {
		this.documentation = UtilImpl.processStringValue(documentation);
	}

	@Override
	public org.skyve.metadata.model.document.Document convert(String metaDataName, AbstractRepository repository) {
		DocumentImpl result = new DocumentImpl(repository);

		// Set document metadata
		String value = getName();
		if (value == null) {
			throw new MetaDataException(metaDataName + " : The document [name] is required");
		}
		result.setName(value);
		result.setExtends(getExtends());
		result.setAbstract(java.lang.Boolean.TRUE.equals(getAbstract()));
		result.setPersistent(getPersistent());
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
			icon = DEFAULT_DOCUMENT_ICON_STYLE_CLASS;
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
		result.setAudited(java.lang.Boolean.FALSE.equals(getAudited()) ? false : true);
		
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
		
		if (bizKeyCode != null) {
			result.setBizKeyMethodCode(bizKeyCode);
		}
		else if (bizKeyExpression != null) {
			StringBuilder sb = new StringBuilder(128);
			sb.append("\t\ttry {\n");
			sb.append("\t\t\treturn org.skyve.util.Binder.formatMessage(org.skyve.CORE.getUser().getCustomer(),\n");
			sb.append("\t\t\t\t\t\t\t\t\t\t\t\t\t\t\"").append(bizKeyExpression).append("\",\n");
			sb.append("\t\t\t\t\t\t\t\t\t\t\t\t\t\tthis);\n");
			sb.append("\t\t}\n");
			sb.append("\t\tcatch (Exception e) {\n");
			sb.append("\t\t\treturn \"Unknown\";\n");
			sb.append("\t\t}");
			result.setBizKeyMethodCode(sb.toString());
			result.setBizKeyExpression(bizKeyExpression);
		}
		else {
			result.setBizKeyMethodCode("\t\treturn toString();\n");
		}

		if ((resultPersistent == null) && java.lang.Boolean.TRUE.equals(result.getParentDatabaseIndex())) {
			throw new MetaDataException(metaDataName + " : The document [parentDocument.index] CANNOT be true for a transient document");
		}
		Set<String> attributeNames = new TreeSet<>();

		// Set interfaces metadata
		if (interfaces != null) {
			for (Interface interfaceMetaData : interfaces) {
				String interfaceName = interfaceMetaData.getInterfaceName();
				if (interfaceName == null || interfaceName.length() < 1) {
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
					throw new MetaDataException(metaDataName + " : The attribute named " + value + " is already an implicit attribute.");
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
				
				// do not allow unicode document names, see https://hibernate.atlassian.net/browse/HHH-13383
				if (!StringUtils.deleteWhitespace(attribute.getName())
						.matches("^([a-zA-Z_$][a-zA-Z\\d_$]*\\.)*[a-zA-Z_$][a-zA-Z\\d_$]*$")) {
					throw new MetaDataException(String.format("%s : %s must only contain non-unicode letters and digits.",
							metaDataName, attribute.getName()));
				}

				// Default auditing to off for view attributes 
				// that don't have an audited value set in their definition.
				if (attribute instanceof AbstractAttribute) {
					AbstractAttribute aa = (AbstractAttribute) attribute;
					if ((aa.getAuditedBool() == null) && UsageType.view.equals(attribute.getUsage())) {
						((AbstractAttribute) attribute).setAudited(false);
					}
				}
				
				AttributeType type = attribute.getAttributeType();
				if (attribute instanceof Field) {
					Field field = (Field) attribute;
					Converter<?> converter = null;
					Class<?> implementingType = type.getImplementingType();
					// NB can't get the actual enumeration type here as the repository is under construction
					// Any default value enumeration type will be a compile error in the domain object anyway.
					
					if ((AttributeType.memo.equals(type) || AttributeType.markup.equals(type)) && 
							(field.getIndex() == null)) {
						field.setIndex(IndexType.textual);
					}

					if (attribute instanceof ConvertableField) {
						ConvertableField convertableField = (ConvertableField) attribute;
						ConverterName converterName = convertableField.getConverterName();
						if (converterName != null) {
							converter = converterName.getConverter();
							if (! type.equals(converter.getAttributeType())) {
								throw new MetaDataException(metaDataName + " : The converter " + converterName + " of field " + attribute.getName() + " is not a converter for type " + type);
							}
							convertableField.setConverter(converter);
						}
					}

					String defaultValue = field.getDefaultValue();
					if (defaultValue != null) {
						try {
							BindUtil.fromString(null, null, implementingType, defaultValue, true);
						} 
						catch (@SuppressWarnings("unused") Exception e) {
							throw new MetaDataException(metaDataName + " : The default value of " + defaultValue + " for field " + field.getName() + " is not coercible to type " + type + ".  Date based types should be expressed as a standard XML date format - YYYY-MM-DD or YYYY-MM-DDTHH24:MM:SS");
						}
					}

					DateValidator dateValidator = null;
					DecimalValidator decimalValidator = null;
					IntegerValidator integerValidator = null;
					LongValidator longValidator = null;
					
					if (field instanceof Text) {
						Text text = (Text) field;
						TextValidator validator = text.getValidator();
						if (validator != null) {
							String regex = validator.getRegularExpression();
							ValidatorType validatorType = validator.getType();
							if ((regex == null) && (validatorType == null)) {
								throw new MetaDataException(metaDataName + " : A regular expression and/or a validator type is required for validator on field " + field.getName());
							}
						}
					}
					else if (field instanceof Date) {
						dateValidator = ((Date) field).getValidator();
					}
					else if (field instanceof DateTime) {
						dateValidator = ((DateTime) field).getValidator();
					}
					else if (field instanceof Time) {
						dateValidator = ((Time) field).getValidator();
					}
					else if (field instanceof Timestamp) {
						dateValidator = ((Timestamp) field).getValidator();
					}
					else if (field instanceof Decimal2) {
						decimalValidator = ((Decimal2) field).getValidator();
						if (decimalValidator != null) {
							java.lang.Integer precision = decimalValidator.getPrecision();
							if ((precision != null) && (precision.intValue() > 2)) {
								throw new MetaDataException(metaDataName + " : Precision for validator on field " + field.getName() + " cannot be > 2");
							}
						}
					}
					else if (field instanceof Decimal5) {
						decimalValidator = ((Decimal5) field).getValidator();
						if (decimalValidator != null) {
							java.lang.Integer precision = decimalValidator.getPrecision();
							if ((precision != null) && (precision.intValue() > 5)) {
								throw new MetaDataException(metaDataName + " : Precision for validator on field " + field.getName() + " cannot be > 5");
							}
						}
					}
					else if (field instanceof Decimal10) {
						decimalValidator = ((Decimal10) field).getValidator();
						if (decimalValidator != null) {
							java.lang.Integer precision = decimalValidator.getPrecision();
							if ((precision != null) && (precision.intValue() > 10)) {
								throw new MetaDataException(metaDataName + " : Precision for validator on field " + field.getName() + " cannot be > 10");
							}
						}
					}
					else if (field instanceof Integer) {
						integerValidator = ((Integer) field).getValidator();
					}
					else if (field instanceof LongInteger) {
						longValidator = ((LongInteger) field).getValidator();
					}
					
					if (dateValidator != null) {
						String xmlMin = dateValidator.getXmlMin();
						String xmlMax = dateValidator.getXmlMax();
						if ((xmlMin == null) && (xmlMax == null)) {
							throw new MetaDataException(metaDataName + " : The validator on field " + attribute.getName() + " must define either a min or max.");
						}
						if (xmlMin != null) {
							try {
								dateValidator.setMin((java.util.Date) BindUtil.fromString(null, converter, implementingType, xmlMin, true));
							} 
							catch (@SuppressWarnings("unused") Exception e) {
								throw new MetaDataException(metaDataName + " : The min value of " + xmlMin + " for validator on field " + field.getName() + 
																" is not coercible to type " + type + 
																".  Values should be specified in the format of the field converter (if defined) or generic formats otherwise - date based types should be expressed as a standard XML date format - YYYY-MM-DD or YYYY-MM-DDTHH24:MM:SS");
							}
						}
						if (xmlMax != null) {
							try {
								dateValidator.setMax((java.util.Date) BindUtil.fromString(null, converter, implementingType, xmlMax, true));
							} 
							catch (@SuppressWarnings("unused") Exception e) {
								throw new MetaDataException(metaDataName + " : The max value of " + xmlMax + " for validator on field " + field.getName() + 
																" is not coercible to type " + type + 
																".  Values should be specified in the format of the field converter (if defined) or generic formats otherwise - date based types should be expressed as a standard XML date format - YYYY-MM-DD or YYYY-MM-DDTHH24:MM:SS");
							}
						}
					}
					if (decimalValidator != null) {
						String xmlMin = decimalValidator.getXmlMin();
						String xmlMax = decimalValidator.getXmlMax();
						if ((xmlMin == null) && (xmlMax == null)) {
							throw new MetaDataException(metaDataName + " : The validator on field " + attribute.getName() + " must define either a min or max.");
						}
						if (xmlMin != null) {
							try {
								decimalValidator.setMin((Decimal) BindUtil.fromString(null, converter, implementingType, xmlMin, true));
							} 
							catch (@SuppressWarnings("unused") Exception e) {
								throw new MetaDataException(metaDataName + " : The min value of " + xmlMin + " for validator on field " + field.getName() + 
																" is not coercible to type " + type + ".  Values should be specified in the format of the field converter (if defined) or generic formats otherwise - decimal based types should be expressed as floating point expressions ie 1.1");
							}
						}
						if (xmlMax != null) {
							try {
								decimalValidator.setMax((Decimal) BindUtil.fromString(null, converter, implementingType, xmlMax, true));
							}
							catch (@SuppressWarnings("unused") Exception e) {
								throw new MetaDataException(metaDataName + " : The max value of " + xmlMax + " for validator on field " + field.getName() + 
										" is not coercible to type " + type + ".  Values should be specified in the format of the field converter (if defined) or generic formats otherwise - decimal based types should be expressed as floating point expressions ie 1.1");
							}
						}
					}
					if (integerValidator != null) {
						String xmlMin = integerValidator.getXmlMin();
						String xmlMax = integerValidator.getXmlMax();
						if ((xmlMin == null) && (xmlMax == null)) {
							throw new MetaDataException(metaDataName + " : The validator on field " + attribute.getName() + " must define either a min or max.");
						}
						if (xmlMin != null) {
							try {
								integerValidator.setMin((java.lang.Integer) BindUtil.fromString(null, converter, implementingType, xmlMin, true));
							} 
							catch (@SuppressWarnings("unused") Exception e) {
								throw new MetaDataException(metaDataName + " : The min value of " + xmlMin + " for validator on field " + field.getName() + 
																" is not coercible to type " + type + ".  Values should be specified in the format of the field converter (if defined) or generic formats otherwise - integer based types should be expressed as integer expressions ie 1");
							}
						}
						if (xmlMax != null) {
							try {
								integerValidator.setMax((java.lang.Integer) BindUtil.fromString(null, converter, implementingType, xmlMax, true));
							}
							catch (@SuppressWarnings("unused") Exception e) {
								throw new MetaDataException(metaDataName + " : The max value of " + xmlMax + " for validator on field " + field.getName() + 
										" is not coercible to type " + type + ".  Values should be specified in the format of the field converter (if defined) or generic formats otherwise - integer based types should be expressed as integer expressions ie 1");
							}
						}
					}
					if (longValidator != null) {
						String xmlMin = longValidator.getXmlMin();
						String xmlMax = longValidator.getXmlMax();
						if ((xmlMin == null) && (xmlMax == null)) {
							throw new MetaDataException(metaDataName + " : The validator on field " + attribute.getName() + " must define either a min or max.");
						}
						if (xmlMin != null) {
							try {
								longValidator.setMin((java.lang.Long) BindUtil.fromString(null, converter, implementingType, xmlMin, true));
							} 
							catch (@SuppressWarnings("unused") Exception e) {
								throw new MetaDataException(metaDataName + " : The min value of " + xmlMin + " for validator on field " + field.getName() + 
																" is not coercible to type " + type + ".  Values should be specified in the format of the field converter (if defined) or generic formats otherwise - long based types should be expressed as long expressions ie 1");
							}
						}
						if (xmlMax != null) {
							try {
								longValidator.setMax((java.lang.Long) BindUtil.fromString(null, converter, implementingType, xmlMax, true));
							}
							catch (@SuppressWarnings("unused") Exception e) {
								throw new MetaDataException(metaDataName + " : The max value of " + xmlMax + " for validator on field " + field.getName() + 
										" is not coercible to type " + type + ".  Values should be specified in the format of the field converter (if defined) or generic formats otherwise - long based types should be expressed as long expressions ie 1");
							}
						}
					}

					if (attribute instanceof LengthField) {
						LengthField lengthField = (LengthField) attribute;
						if (lengthField.getLength() < 1) {
							throw new MetaDataException(metaDataName + " : The length of field " + attribute.getName() + " is not a valid length");
						}
					}

					if (attribute instanceof Enumeration) {
						Enumeration enumeration = (Enumeration) attribute;
						enumeration.setRepository(repository);
						
						// Enumeration can be defined inline (ie a new one) or
						// a reference (module, document, attribute) to another definition
						String moduleRef = enumeration.getModuleRef();
						String documentRef = enumeration.getDocumentRef();
						String attributeRef = enumeration.getAttributeRef();
						List<EnumeratedValue> values = enumeration.getXmlValues();
						
						if ((moduleRef != null) || (documentRef != null) || (attributeRef != null)) { // reference
							if (attributeRef == null) {
								throw new MetaDataException(metaDataName + " : Enumeration " + attribute.getName() + 
																" is defined as a reference to another enum but [attributeRef] is not defined.");
							}
							if ((moduleRef != null) && (documentRef == null)) {
								throw new MetaDataException(metaDataName + " : Enumeration " + attribute.getName() + 
																" has a [moduleRef] but no documentRef.");
							}
							if (! values.isEmpty()) {
								throw new MetaDataException(metaDataName + " : Enumeration " + attribute.getName() + 
																" is defined as a reference to another enum but has [values] defined.");
							}
							if (enumeration.getXmlTypeName() != null) {
								throw new MetaDataException(metaDataName + " : Enumeration " + attribute.getName() + 
																" is defined as a reference to another enum but has [typeName] defined.");
							}
						}
						else { // definition
							if (values.isEmpty()) {
								throw new MetaDataException(metaDataName + " : Enumeration " + attribute.getName() + 
																" has no [values] defined.");
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
				else if (attribute instanceof Relation) {
					Relation relation = (Relation) attribute;
					value = relation.getDocumentName();
					if (value == null) {
						throw new MetaDataException(metaDataName + " : The relation [documentName] is required for relation " + relation.getName());
					}

					if (relation instanceof Association) {
						Association association = (Association) relation;
						if (association.getType() == null) {
							throw new MetaDataException(metaDataName + " : The association [type] is required for association " +
															relation.getName());
						}
						if ((! association.isPersistent()) && (association.getDatabaseIndex() != null)) {
							throw new MetaDataException(metaDataName + " : The association [databaseIndex] is NOT required for transient association " +
															relation.getName());
						}
					}
					else if (relation instanceof Collection) {
						Collection collection = (Collection) relation;
						if (collection.getMinCardinality() == null) {
							throw new MetaDataException(metaDataName + " : The collection [minCardinality] is required for collection " + 
															relation.getName());
						}

						java.lang.Boolean ownerDatabaseIndex = collection.getOwnerDatabaseIndex();
						java.lang.Boolean elementDatabaseIndex = collection.getElementDatabaseIndex();

						// Check for compound bindings.
						// Hibernate defines collection sort order in terms of SQL columns in the ORM.
						// This means, no compound bindings and references must actually be to foreign key columns.
						// We need to indicate to the framework that we will need to order the collection ourselves in memory.
						if (collection.isPersistent()) {
							List<Ordering> orderings = collection.getOrdering();
							if (orderings != null) {
								for (Ordering ordering : orderings) {
									String by = ordering.getBy();
									if (by.indexOf('.') >= 0) {
										((CollectionImpl) collection).setComplexOrdering(true);
										break;
									}
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
				if (conditionName.startsWith("not")) {
					throw new MetaDataException(metaDataName + " : Condition name " + conditionName + " cannot start with 'not'.  The negated condition will be generated automatically.");
				}
				if (! attributeNames.add(conditionName)) {
					throw new MetaDataException(metaDataName + " : Condition name clashes with field/association/reference/condition named " + 
													conditionName);
				}
				String conditionExpression = conditionMetaData.getExpression();
				if (conditionExpression == null) {
					throw new MetaDataException(metaDataName + " : A condition [expression] is required.");
				}

				ConditionImpl condition = new ConditionImpl();
				condition.setExpression(conditionExpression);
				condition.setDocumentation(conditionMetaData.getDocumentation());
				condition.setDescription(conditionMetaData.getDescription());
				condition.setUsage(condition.getUsage());
				
				if (result.getConditions().put(conditionName, condition) != null) {
					throw new MetaDataException(metaDataName + " : A duplicate condition of " + conditionName + " is defined.");
				}
			}
		}

		// Set unique constraints.
		if (uniqueConstraints != null) {
			Set<String> constraintNames = new TreeSet<>();

			for (UniqueConstraint constraintMetaData : uniqueConstraints) {
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

				result.putUniqueConstraint(constraint);
			}
		}

		result.setDocumentation(getDocumentation());
		
		return result;
	}
}
