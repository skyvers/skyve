package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.InterfaceImpl;
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
import org.skyve.impl.metadata.repository.document.DocumentMetaData;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Extends;
import org.skyve.metadata.model.Persistent;
import org.skyve.metadata.model.document.Association;
import org.skyve.metadata.model.document.Collection;
import org.skyve.metadata.model.document.Condition;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.Interface;
import org.skyve.metadata.model.document.UniqueConstraint;

public class FluentDocument {
	private DocumentMetaData document = null;
	
	public FluentDocument() {
		document = new DocumentMetaData();
	}

	public FluentDocument(DocumentMetaData document) {
		this.document = document;
	}

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
			parent.databaseIndex(! Boolean.FALSE.equals(impl.getParentDatabaseIndex()));
			parentDocument(parent);
		}
		
		Persistent persistent = document.getPersistent();
		if (persistent != null) {
			persistent(new FluentPersistent().from(persistent));
		}
		
		dynamic(document.isDynamic());
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
			if (attribute instanceof Text) {
				addText(new FluentText().from((Text) attribute));
			}
			else if (attribute instanceof org.skyve.impl.metadata.model.document.field.Boolean) {
				addBoolean(new FluentBoolean().from((org.skyve.impl.metadata.model.document.field.Boolean) attribute));
			}
			else if (attribute instanceof Enumeration) {
				addEnumeration(new FluentEnumeration().from((Enumeration) attribute));
			}
			else if (attribute instanceof Memo) {
				addMemo(new FluentMemo().from((Memo) attribute));
			}
			else if (attribute instanceof Date) {
				addDate(new FluentDate().from((Date) attribute));
			}
			else if (attribute instanceof org.skyve.impl.metadata.model.document.field.Integer) {
				addInteger(new FluentInteger().from((org.skyve.impl.metadata.model.document.field.Integer) attribute));
			}
			else if (attribute instanceof Association) {
				addAssociation(new FluentAssociation().from((Association) attribute));
			}
			else if (attribute instanceof Collection) {
				addCollection(new FluentCollection().from((Collection) attribute));
			}
			else if (attribute instanceof LongInteger) {
				addLongInteger(new FluentLongInteger().from((LongInteger) attribute));
			}
			else if (attribute instanceof Decimal2) {
				addDecimal2(new FluentDecimal2().from((Decimal2) attribute));
			}
			else if (attribute instanceof Decimal5) {
				addDecimal5(new FluentDecimal5().from((Decimal5) attribute));
			}
			else if (attribute instanceof Decimal10) {
				addDecimal10(new FluentDecimal10().from((Decimal10) attribute));
			}
			else if (attribute instanceof Time) {
				addTime(new FluentTime().from((Time) attribute));
			}
			else if (attribute instanceof DateTime) {
				addDateTime(new FluentDateTime().from((DateTime) attribute));
			}
			else if (attribute instanceof Timestamp) {
				addTimestamp(new FluentTimestamp().from((Timestamp) attribute));
			}
			else if (attribute instanceof Markup) {
				addMarkup(new FluentMarkup().from((Markup) attribute));
			}
			else if (attribute instanceof Colour) {
				addColour(new FluentColour().from((Colour) attribute));
			}
			else if (attribute instanceof Content) {
				addContent(new FluentContent().from((Content) attribute));
			}
			else if (attribute instanceof Image) {
				addImage(new FluentImage().from((Image) attribute));
			}
			else if (attribute instanceof Geometry) {
				addGeometry(new FluentGeometry().from((Geometry) attribute));
			}
			else if (attribute instanceof Id) {
				addId(new FluentId().from((Id) attribute));
			}
			else if (attribute instanceof InverseOne) {
				addInverseOne(new FluentInverseOne().from((InverseOne) attribute));
			}
			else if (attribute instanceof InverseMany) {
				addInverseMany(new FluentInverseMany().from((InverseMany) attribute));
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

	public FluentDocument name(String name) {
		document.setName(name);
		return this;
	}
	
	public FluentDocument abstractDocument(boolean abstractDocument) {
		document.setAbstract(abstractDocument ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	public FluentDocument documentation(String documentation) {
		document.setDocumentation(documentation);
		return this;
	}

	public FluentDocument extendsDocument(String baseDocumentName) {
		Extends inherits = new Extends();
		inherits.setDocumentName(baseDocumentName);
		document.setExtends(inherits);
		return this;
	}

	public FluentDocument parentDocument(FluentParentDocument parent) {
		document.setParentDocument(parent.get());
		return this;
	}

	public FluentDocument persistent(FluentPersistent persistent) {
		document.setPersistent(persistent.get());
		return this;
	}
	
	public FluentDocument dynamic(boolean dynamic) {
		document.setDynamic(dynamic ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	public FluentDocument singularAlias(String singularAlias) {
		document.setSingularAlias(singularAlias);
		return this;
	}

	public FluentDocument pluralAlias(String pluralAlias) {
		document.setPluralAlias(pluralAlias);
		return this;
	}

	public FluentDocument description(String description) {
		document.setDescription(description);
		return this;
	}

	public FluentDocument bizKeyExpression(String expression) {
		BizKey bizKey = new BizKey();
		bizKey.setExpression(expression);
		document.setBizKey(bizKey);
		return this;
	}
	
	public FluentDocument iconStyleClass(String iconStyleClass) {
		document.setIconStyleClass(iconStyleClass);
		return this;
	}
	
	public FluentDocument icon16x16RelativeFilePath(String icon16x16RelativeFilePath) {
		document.setIcon16x16RelativeFilePath(icon16x16RelativeFilePath);
		return this;
	}
	
	public FluentDocument icon32x32RelativeFilePath(String icon32x32RelativeFilePath) {
		document.setIcon32x32RelativeFilePath(icon32x32RelativeFilePath);
		return this;
	}
	
	public FluentDocument audited(boolean audited) {
		document.setAudited(audited ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	public FluentDocument addImplementingInterface(String fullyQualifiedInterfaceClassName) {
		InterfaceImpl i = new InterfaceImpl();
		i.setInterfaceName(fullyQualifiedInterfaceClassName);
		document.getImplements().add(i);
		return this;
	}

	public FluentDocument addText(FluentText text) {
		document.getAttributes().add(text.get());
		return this;
	}

	public FluentDocument addDate(FluentDate date) {
		document.getAttributes().add(date.get());
		return this;
	}
	
	public FluentDocument addTime(FluentTime time) {
		document.getAttributes().add(time.get());
		return this;
	}

	public FluentDocument addDateTime(FluentDateTime dateTime) {
		document.getAttributes().add(dateTime.get());
		return this;
	}

	public FluentDocument addTimestamp(FluentTimestamp timestamp) {
		document.getAttributes().add(timestamp.get());
		return this;
	}
	
	public FluentDocument addInteger(FluentInteger integer) {
		document.getAttributes().add(integer.get());
		return this;
	}
	
	public FluentDocument addLongInteger(FluentLongInteger longInteger) {
		document.getAttributes().add(longInteger.get());
		return this;
	}

	public FluentDocument addDecimal2(FluentDecimal2 decimal) {
		document.getAttributes().add(decimal.get());
		return this;
	}
	
	public FluentDocument addDecimal5(FluentDecimal5 decimal) {
		document.getAttributes().add(decimal.get());
		return this;
	}
	
	public FluentDocument addDecimal10(FluentDecimal10 decimal) {
		document.getAttributes().add(decimal.get());
		return this;
	}
	
	public FluentDocument addBoolean(FluentBoolean bool) {
		document.getAttributes().add(bool.get());
		return this;
	}

	public FluentDocument addEnumeration(FluentEnumeration enumeration) {
		document.getAttributes().add(enumeration.get());
		return this;
	}
	
	public FluentDocument addMemo(FluentMemo memo) {
		document.getAttributes().add(memo.get());
		return this;
	}

	public FluentDocument addMarkup(FluentMarkup markup) {
		document.getAttributes().add(markup.get());
		return this;
	}

	public FluentDocument addColour(FluentColour colour) {
		document.getAttributes().add(colour.get());
		return this;
	}
	
	public FluentDocument addContent(FluentContent content) {
		document.getAttributes().add(content.get());
		return this;
	}

	public FluentDocument addImage(FluentImage image) {
		document.getAttributes().add(image.get());
		return this;
	}
	
	public FluentDocument addGeometry(FluentGeometry geometry) {
		document.getAttributes().add(geometry.get());
		return this;
	}

	public FluentDocument addId(FluentId id) {
		document.getAttributes().add(id.get());
		return this;
	}

	public FluentDocument addAssociation(FluentAssociation association) {
		document.getAttributes().add(association.get());
		return this;
	}

	public FluentDocument addCollection(FluentCollection collection) {
		document.getAttributes().add(collection.get());
		return this;
	}
	
	public FluentDocument addInverseOne(FluentInverseOne inverseOne) {
		document.getAttributes().add(inverseOne.get());
		return this;
	}

	public FluentDocument addInverseMany(FluentInverseMany inverseMany) {
		document.getAttributes().add(inverseMany.get());
		return this;
	}
	
	public FluentDocument addCondition(FluentCondition condition) {
		document.getConditions().add(condition.get());
		return this;
	}
	
	public FluentDocument addUniqueConstraint(FluentDocumentUniqueConstraint constraint) {
		document.getUniqueConstraints().add(constraint.get());
		return this;
	}
	
	public DocumentMetaData get() {
		return document;
	}
}
