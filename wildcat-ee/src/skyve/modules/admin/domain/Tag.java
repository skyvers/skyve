package modules.admin.domain;

import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;
import org.skyve.CORE;
import org.skyve.domain.types.Enumeration;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.wildcat.domain.AbstractPersistentBean;

/**
 * Tag
 * 
 * @depend - - - CombinationsOperator
 * @depend - - - FilterOperator
 * @depend - - - FilterAction
 * @navhas n actionTag 0..1 Tag
 * @navhas n copyToUser 0..1 User
 * @stereotype "persistent"
 */
@XmlType
@XmlRootElement
public class Tag extends AbstractPersistentBean {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "admin";
	/** @hidden */
	public static final String DOCUMENT_NAME = "Tag";

	/** @hidden */
	public static final String namePropertyName = "name";
	/** @hidden */
	public static final String visiblePropertyName = "visible";
	/** @hidden */
	public static final String combinationsOperatorPropertyName = "combinationsOperator";
	/** @hidden */
	public static final String combinationExplanationPropertyName = "combinationExplanation";
	/** @hidden */
	public static final String currentTagCountPropertyName = "currentTagCount";
	/** @hidden */
	public static final String actionTagCountPropertyName = "actionTagCount";
	/** @hidden */
	public static final String copyToUserPropertyName = "copyToUser";
	/** @hidden */
	public static final String copyToUserTagNamePropertyName = "copyToUserTagName";
	/** @hidden */
	public static final String moduleNamePropertyName = "moduleName";
	/** @hidden */
	public static final String documentNamePropertyName = "documentName";
	/** @hidden */
	public static final String attributeNamePropertyName = "attributeName";
	/** @hidden */
	public static final String fileHasHeadersPropertyName = "fileHasHeaders";
	/** @hidden */
	public static final String numberLoadedPropertyName = "numberLoaded";
	/** @hidden */
	public static final String numberMatchedPropertyName = "numberMatched";
	/** @hidden */
	public static final String numberTaggedPropertyName = "numberTagged";
	/** @hidden */
	public static final String filterOperatorPropertyName = "filterOperator";
	/** @hidden */
	public static final String filterActionPropertyName = "filterAction";
	/** @hidden */
	public static final String unTagSuccessfulPropertyName = "unTagSuccessful";
	/** @hidden */
	public static final String filterColumnPropertyName = "filterColumn";
	/** @hidden */
	public static final String actionTagPropertyName = "actionTag";
	/** @hidden */
	public static final String documentConditionPropertyName = "documentCondition";
	/** @hidden */
	public static final String documentActionPropertyName = "documentAction";
	/** @hidden */
	public static final String documentActionResultsPropertyName = "documentActionResults";

	/**
	 * Operator
	 **/
	@XmlEnum
	public static enum CombinationsOperator implements Enumeration {
		union("Union", "Union"),
		except("Except", "Except"),
		intersect("Intersect", "Intersect");

		private String code;
		private String description;

		/** @hidden */
		private static List<DomainValue> domainValues;

		private CombinationsOperator(String code, String description) {
			this.code = code;
			this.description = description;
		}

		@Override
		public String toCode() {
			return code;
		}

		@Override
		public String toDescription() {
			return description;
		}

		public static CombinationsOperator fromCode(String code) {
			CombinationsOperator result = null;

			for (CombinationsOperator value : values()) {
				if (value.code.equals(code)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static CombinationsOperator fromDescription(String description) {
			CombinationsOperator result = null;

			for (CombinationsOperator value : values()) {
				if (value.description.equals(description)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static List<DomainValue> toDomainValues() {
			if (domainValues == null) {
				CombinationsOperator[] values = values();
				domainValues = new ArrayList<>(values.length);
				for (CombinationsOperator value : values) {
					domainValues.add(new DomainValue(value.code, value.description));
				}
			}

			return domainValues;
		}
	}

	/**
	 * Filter Operator
	 **/
	@XmlEnum
	public static enum FilterOperator implements Enumeration {
		equals("equals", "Equals"),
		like("like", "Like"),
		contains("contains", "Contains");

		private String code;
		private String description;

		/** @hidden */
		private static List<DomainValue> domainValues;

		private FilterOperator(String code, String description) {
			this.code = code;
			this.description = description;
		}

		@Override
		public String toCode() {
			return code;
		}

		@Override
		public String toDescription() {
			return description;
		}

		public static FilterOperator fromCode(String code) {
			FilterOperator result = null;

			for (FilterOperator value : values()) {
				if (value.code.equals(code)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static FilterOperator fromDescription(String description) {
			FilterOperator result = null;

			for (FilterOperator value : values()) {
				if (value.description.equals(description)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static List<DomainValue> toDomainValues() {
			if (domainValues == null) {
				FilterOperator[] values = values();
				domainValues = new ArrayList<>(values.length);
				for (FilterOperator value : values) {
					domainValues.add(new DomainValue(value.code, value.description));
				}
			}

			return domainValues;
		}
	}

	/**
	 * Filter Action
	 **/
	@XmlEnum
	public static enum FilterAction implements Enumeration {
		tagRecordsThatMatch("tag", "Tag records that match"),
		unTagRecordsThatMatch("unTag", "UnTag records that match");

		private String code;
		private String description;

		/** @hidden */
		private static List<DomainValue> domainValues;

		private FilterAction(String code, String description) {
			this.code = code;
			this.description = description;
		}

		@Override
		public String toCode() {
			return code;
		}

		@Override
		public String toDescription() {
			return description;
		}

		public static FilterAction fromCode(String code) {
			FilterAction result = null;

			for (FilterAction value : values()) {
				if (value.code.equals(code)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static FilterAction fromDescription(String description) {
			FilterAction result = null;

			for (FilterAction value : values()) {
				if (value.description.equals(description)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static List<DomainValue> toDomainValues() {
			if (domainValues == null) {
				FilterAction[] values = values();
				domainValues = new ArrayList<>(values.length);
				for (FilterAction value : values) {
					domainValues.add(new DomainValue(value.code, value.description));
				}
			}

			return domainValues;
		}
	}

	private String name;
	private Boolean visible;
	private CombinationsOperator combinationsOperator;
	private String combinationExplanation;
	private Integer currentTagCount;
	private Integer actionTagCount;
	private User copyToUser = null;
	private String copyToUserTagName;
	/**
	 * The module to tag.
	 **/
	private String moduleName;
	/**
	 * The document to tag.
	 **/
	private String documentName;
	/**
	 * The name of the attribute to tag.
	 **/
	private String attributeName;
	private Boolean fileHasHeaders;
	private Integer numberLoaded;
	private Integer numberMatched;
	private Integer numberTagged;
	private FilterOperator filterOperator;
	private FilterAction filterAction;
	private Boolean unTagSuccessful;
	private Integer filterColumn;
	/**
	 * The other tag to use for the action to be performed on this tag.
	 **/
	private Tag actionTag = null;
	/**
	 * The condition which must be satisfied for the tagged document, for the action to be executed.
	 **/
	private String documentCondition;
	/**
	 * The action to be executed on the tagged document (provided the specified condition is satisfied for that document).
	 **/
	private String documentAction;
	private String documentActionResults;

	@Override
	@XmlTransient
	public String getBizModule() {
		return Tag.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return Tag.DOCUMENT_NAME;
	}

	public static Tag newInstance() throws Exception {
		return CORE.getUser().getCustomer().getModule(MODULE_NAME).getDocument(CORE.getUser().getCustomer(), DOCUMENT_NAME).newInstance(CORE.getUser());
	}

	@Override
	@XmlTransient
	public String getBizKey() {
		try {
			return org.skyve.util.Binder.formatMessage(org.skyve.CORE.getUser().getCustomer(),
														"{name}",
														this);
		}
		catch (Exception e) {
			return "Unknown";
		}
	}

	@Override
	public boolean equals(Object o) {
		return ((o instanceof Tag) && 
					this.getBizId().equals(((Tag) o).getBizId()));
	}

	/**
	 * {@link #name} accessor.
	 **/
	public String getName() {
		return name;
	}

	/**
	 * {@link #name} mutator.
	 * 
	 * @param name	The new value to set.
	 **/
	@XmlElement
	public void setName(String name) {
		preset(namePropertyName, name);
		this.name = name;
	}

	/**
	 * {@link #visible} accessor.
	 **/
	public Boolean getVisible() {
		return visible;
	}

	/**
	 * {@link #visible} mutator.
	 * 
	 * @param visible	The new value to set.
	 **/
	@XmlElement
	public void setVisible(Boolean visible) {
		preset(visiblePropertyName, visible);
		this.visible = visible;
	}

	/**
	 * {@link #combinationsOperator} accessor.
	 **/
	public CombinationsOperator getCombinationsOperator() {
		return combinationsOperator;
	}

	/**
	 * {@link #combinationsOperator} mutator.
	 * 
	 * @param combinationsOperator	The new value to set.
	 **/
	@XmlElement
	public void setCombinationsOperator(CombinationsOperator combinationsOperator) {
		this.combinationsOperator = combinationsOperator;
	}

	/**
	 * {@link #combinationExplanation} accessor.
	 **/
	public String getCombinationExplanation() {
		return combinationExplanation;
	}

	/**
	 * {@link #combinationExplanation} mutator.
	 * 
	 * @param combinationExplanation	The new value to set.
	 **/
	@XmlElement
	public void setCombinationExplanation(String combinationExplanation) {
		this.combinationExplanation = combinationExplanation;
	}

	/**
	 * {@link #currentTagCount} accessor.
	 **/
	public Integer getCurrentTagCount() {
		return currentTagCount;
	}

	/**
	 * {@link #currentTagCount} mutator.
	 * 
	 * @param currentTagCount	The new value to set.
	 **/
	@XmlElement
	public void setCurrentTagCount(Integer currentTagCount) {
		this.currentTagCount = currentTagCount;
	}

	/**
	 * {@link #actionTagCount} accessor.
	 **/
	public Integer getActionTagCount() {
		return actionTagCount;
	}

	/**
	 * {@link #actionTagCount} mutator.
	 * 
	 * @param actionTagCount	The new value to set.
	 **/
	@XmlElement
	public void setActionTagCount(Integer actionTagCount) {
		this.actionTagCount = actionTagCount;
	}

	/**
	 * {@link #copyToUser} accessor.
	 **/
	public User getCopyToUser() {
		return copyToUser;
	}

	/**
	 * {@link #copyToUser} mutator.
	 * 
	 * @param copyToUser	The new value to set.
	 **/
	@XmlElement
	public void setCopyToUser(User copyToUser) {
		this.copyToUser = copyToUser;
	}

	/**
	 * {@link #copyToUserTagName} accessor.
	 **/
	public String getCopyToUserTagName() {
		return copyToUserTagName;
	}

	/**
	 * {@link #copyToUserTagName} mutator.
	 * 
	 * @param copyToUserTagName	The new value to set.
	 **/
	@XmlElement
	public void setCopyToUserTagName(String copyToUserTagName) {
		this.copyToUserTagName = copyToUserTagName;
	}

	/**
	 * {@link #moduleName} accessor.
	 **/
	public String getModuleName() {
		return moduleName;
	}

	/**
	 * {@link #moduleName} mutator.
	 * 
	 * @param moduleName	The new value to set.
	 **/
	@XmlElement
	public void setModuleName(String moduleName) {
		preset(moduleNamePropertyName, moduleName);
		this.moduleName = moduleName;
	}

	/**
	 * {@link #documentName} accessor.
	 **/
	public String getDocumentName() {
		return documentName;
	}

	/**
	 * {@link #documentName} mutator.
	 * 
	 * @param documentName	The new value to set.
	 **/
	@XmlElement
	public void setDocumentName(String documentName) {
		preset(documentNamePropertyName, documentName);
		this.documentName = documentName;
	}

	/**
	 * {@link #attributeName} accessor.
	 **/
	public String getAttributeName() {
		return attributeName;
	}

	/**
	 * {@link #attributeName} mutator.
	 * 
	 * @param attributeName	The new value to set.
	 **/
	@XmlElement
	public void setAttributeName(String attributeName) {
		preset(attributeNamePropertyName, attributeName);
		this.attributeName = attributeName;
	}

	/**
	 * {@link #fileHasHeaders} accessor.
	 **/
	public Boolean getFileHasHeaders() {
		return fileHasHeaders;
	}

	/**
	 * {@link #fileHasHeaders} mutator.
	 * 
	 * @param fileHasHeaders	The new value to set.
	 **/
	@XmlElement
	public void setFileHasHeaders(Boolean fileHasHeaders) {
		this.fileHasHeaders = fileHasHeaders;
	}

	/**
	 * {@link #numberLoaded} accessor.
	 **/
	public Integer getNumberLoaded() {
		return numberLoaded;
	}

	/**
	 * {@link #numberLoaded} mutator.
	 * 
	 * @param numberLoaded	The new value to set.
	 **/
	@XmlElement
	public void setNumberLoaded(Integer numberLoaded) {
		this.numberLoaded = numberLoaded;
	}

	/**
	 * {@link #numberMatched} accessor.
	 **/
	public Integer getNumberMatched() {
		return numberMatched;
	}

	/**
	 * {@link #numberMatched} mutator.
	 * 
	 * @param numberMatched	The new value to set.
	 **/
	@XmlElement
	public void setNumberMatched(Integer numberMatched) {
		this.numberMatched = numberMatched;
	}

	/**
	 * {@link #numberTagged} accessor.
	 **/
	public Integer getNumberTagged() {
		return numberTagged;
	}

	/**
	 * {@link #numberTagged} mutator.
	 * 
	 * @param numberTagged	The new value to set.
	 **/
	@XmlElement
	public void setNumberTagged(Integer numberTagged) {
		this.numberTagged = numberTagged;
	}

	/**
	 * {@link #filterOperator} accessor.
	 **/
	public FilterOperator getFilterOperator() {
		return filterOperator;
	}

	/**
	 * {@link #filterOperator} mutator.
	 * 
	 * @param filterOperator	The new value to set.
	 **/
	@XmlElement
	public void setFilterOperator(FilterOperator filterOperator) {
		this.filterOperator = filterOperator;
	}

	/**
	 * {@link #filterAction} accessor.
	 **/
	public FilterAction getFilterAction() {
		return filterAction;
	}

	/**
	 * {@link #filterAction} mutator.
	 * 
	 * @param filterAction	The new value to set.
	 **/
	@XmlElement
	public void setFilterAction(FilterAction filterAction) {
		this.filterAction = filterAction;
	}

	/**
	 * {@link #unTagSuccessful} accessor.
	 **/
	public Boolean getUnTagSuccessful() {
		return unTagSuccessful;
	}

	/**
	 * {@link #unTagSuccessful} mutator.
	 * 
	 * @param unTagSuccessful	The new value to set.
	 **/
	@XmlElement
	public void setUnTagSuccessful(Boolean unTagSuccessful) {
		this.unTagSuccessful = unTagSuccessful;
	}

	/**
	 * {@link #filterColumn} accessor.
	 **/
	public Integer getFilterColumn() {
		return filterColumn;
	}

	/**
	 * {@link #filterColumn} mutator.
	 * 
	 * @param filterColumn	The new value to set.
	 **/
	@XmlElement
	public void setFilterColumn(Integer filterColumn) {
		this.filterColumn = filterColumn;
	}

	/**
	 * {@link #actionTag} accessor.
	 **/
	public Tag getActionTag() {
		return actionTag;
	}

	/**
	 * {@link #actionTag} mutator.
	 * 
	 * @param actionTag	The new value to set.
	 **/
	@XmlElement
	public void setActionTag(Tag actionTag) {
		this.actionTag = actionTag;
	}

	/**
	 * {@link #documentCondition} accessor.
	 **/
	public String getDocumentCondition() {
		return documentCondition;
	}

	/**
	 * {@link #documentCondition} mutator.
	 * 
	 * @param documentCondition	The new value to set.
	 **/
	@XmlElement
	public void setDocumentCondition(String documentCondition) {
		this.documentCondition = documentCondition;
	}

	/**
	 * {@link #documentAction} accessor.
	 **/
	public String getDocumentAction() {
		return documentAction;
	}

	/**
	 * {@link #documentAction} mutator.
	 * 
	 * @param documentAction	The new value to set.
	 **/
	@XmlElement
	public void setDocumentAction(String documentAction) {
		this.documentAction = documentAction;
	}

	/**
	 * {@link #documentActionResults} accessor.
	 **/
	public String getDocumentActionResults() {
		return documentActionResults;
	}

	/**
	 * {@link #documentActionResults} mutator.
	 * 
	 * @param documentActionResults	The new value to set.
	 **/
	@XmlElement
	public void setDocumentActionResults(String documentActionResults) {
		this.documentActionResults = documentActionResults;
	}

	@XmlTransient
	public boolean isAttributeSet() {
		return (getAttributeName()!=null);
	}

	public boolean isNotAttributeSet() {
		return (! isAttributeSet());
	}

	@XmlTransient
	public boolean isDocumentSet() {
		return (getModuleName()!=null && getDocumentName()!=null);
	}

	public boolean isNotDocumentSet() {
		return (! isDocumentSet());
	}

	@XmlTransient
	public boolean isFileLoaded() {
		return (getNumberLoaded().intValue()>0);
	}

	public boolean isNotFileLoaded() {
		return (! isFileLoaded());
	}

	@XmlTransient
	public boolean isModuleSet() {
		return (getModuleName()!=null);
	}

	public boolean isNotModuleSet() {
		return (! isModuleSet());
	}
}
