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
import org.skyve.impl.domain.AbstractPersistentBean;
import org.skyve.metadata.model.document.Bizlet.DomainValue;

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
	public static final String uploadModuleNamePropertyName = "uploadModuleName";
	/** @hidden */
	public static final String uploadDocumentNamePropertyName = "uploadDocumentName";
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
	public static final String actionModuleNamePropertyName = "actionModuleName";
	/** @hidden */
	public static final String actionDocumentNamePropertyName = "actionDocumentName";
	/** @hidden */
	public static final String unTagSuccessfulPropertyName = "unTagSuccessful";
	/** @hidden */
	public static final String notificationPropertyName = "notification";
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
		private DomainValue domainValue;

		/** @hidden */
		private static List<DomainValue> domainValues;

		private CombinationsOperator(String code, String description) {
			this.code = code;
			this.description = description;
			this.domainValue = new DomainValue(code, description);
		}

		@Override
		public String toCode() {
			return code;
		}

		@Override
		public String toDescription() {
			return description;
		}

		@Override
		public DomainValue toDomainValue() {
			return domainValue;
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
					domainValues.add(value.domainValue);
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
		private DomainValue domainValue;

		/** @hidden */
		private static List<DomainValue> domainValues;

		private FilterOperator(String code, String description) {
			this.code = code;
			this.description = description;
			this.domainValue = new DomainValue(code, description);
		}

		@Override
		public String toCode() {
			return code;
		}

		@Override
		public String toDescription() {
			return description;
		}

		@Override
		public DomainValue toDomainValue() {
			return domainValue;
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
					domainValues.add(value.domainValue);
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
		private DomainValue domainValue;

		/** @hidden */
		private static List<DomainValue> domainValues;

		private FilterAction(String code, String description) {
			this.code = code;
			this.description = description;
			this.domainValue = new DomainValue(code, description);
		}

		@Override
		public String toCode() {
			return code;
		}

		@Override
		public String toDescription() {
			return description;
		}

		@Override
		public DomainValue toDomainValue() {
			return domainValue;
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
					domainValues.add(value.domainValue);
				}
			}

			return domainValues;
		}
	}

	/**
	 * Name
	 **/
	private String name;
	/**
	 * Visible
	 **/
	private Boolean visible;
	/**
	 * Operator
	 **/
	private CombinationsOperator combinationsOperator;
	/**
	 * Explanation
	 **/
	private String combinationExplanation;
	/**
	 * Current Tag Count
	 **/
	private Integer currentTagCount;
	/**
	 * Other Tag Count
	 **/
	private Integer actionTagCount;
	/**
	 * Copy to user
	 **/
	private User copyToUser = null;
	/**
	 * Tag Name
	 **/
	private String copyToUserTagName;
	/**
	 * Module
	 * <br/>
	 * The module to tag.
	 **/
	private String uploadModuleName;
	/**
	 * Document
	 * <br/>
	 * The document to tag.
	 **/
	private String uploadDocumentName;
	/**
	 * Attribute
	 * <br/>
	 * The name of the attribute to tag.
	 **/
	private String attributeName;
	/**
	 * Upload File has Column Headers
	 **/
	private Boolean fileHasHeaders;
	/**
	 * Number of loaded values
	 **/
	private Integer numberLoaded;
	/**
	 * Number of matching records
	 **/
	private Integer numberMatched;
	/**
	 * Number of tagged records
	 **/
	private Integer numberTagged;
	/**
	 * Filter Operator
	 **/
	private FilterOperator filterOperator;
	/**
	 * Filter Action
	 **/
	private FilterAction filterAction;
	/**
	 * Module
	 * <br/>
	 * The module for the tag Action.
	 **/
	private String actionModuleName;
	/**
	 * Document
	 * <br/>
	 * The document for the tag Action.
	 **/
	private String actionDocumentName;
	/**
	 * Untag successful documents
	 **/
	private Boolean unTagSuccessful;
	/**
	 * Notify when job is complete
	 **/
	private Boolean notification;
	/**
	 * Column (first column is 1))
	 **/
	private Integer filterColumn;
	/**
	 * Other Tag
	 * <br/>
	 * The other tag to use for the action to be performed on this tag.
	 **/
	private Tag actionTag = null;
	/**
	 * Condition
	 * <br/>
	 * The condition which must be satisfied for the tagged document, for the action to be executed.
	 **/
	private String documentCondition;
	/**
	 * Action
	 * <br/>
	 * The action to be executed on the tagged document (provided the specified condition is satisfied for that document).
	 **/
	private String documentAction;
	/**
	 * Request
	 **/
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
	 * @return	The value.
	 **/
	public String getName() {
		return name;
	}

	/**
	 * {@link #name} mutator.
	 * @param name	The new value.
	 **/
	@XmlElement
	public void setName(String name) {
		preset(namePropertyName, name);
		this.name = name;
	}

	/**
	 * {@link #visible} accessor.
	 * @return	The value.
	 **/
	public Boolean getVisible() {
		return visible;
	}

	/**
	 * {@link #visible} mutator.
	 * @param visible	The new value.
	 **/
	@XmlElement
	public void setVisible(Boolean visible) {
		preset(visiblePropertyName, visible);
		this.visible = visible;
	}

	/**
	 * {@link #combinationsOperator} accessor.
	 * @return	The value.
	 **/
	public CombinationsOperator getCombinationsOperator() {
		return combinationsOperator;
	}

	/**
	 * {@link #combinationsOperator} mutator.
	 * @param combinationsOperator	The new value.
	 **/
	@XmlElement
	public void setCombinationsOperator(CombinationsOperator combinationsOperator) {
		this.combinationsOperator = combinationsOperator;
	}

	/**
	 * {@link #combinationExplanation} accessor.
	 * @return	The value.
	 **/
	public String getCombinationExplanation() {
		return combinationExplanation;
	}

	/**
	 * {@link #combinationExplanation} mutator.
	 * @param combinationExplanation	The new value.
	 **/
	@XmlElement
	public void setCombinationExplanation(String combinationExplanation) {
		this.combinationExplanation = combinationExplanation;
	}

	/**
	 * {@link #currentTagCount} accessor.
	 * @return	The value.
	 **/
	public Integer getCurrentTagCount() {
		return currentTagCount;
	}

	/**
	 * {@link #currentTagCount} mutator.
	 * @param currentTagCount	The new value.
	 **/
	@XmlElement
	public void setCurrentTagCount(Integer currentTagCount) {
		this.currentTagCount = currentTagCount;
	}

	/**
	 * {@link #actionTagCount} accessor.
	 * @return	The value.
	 **/
	public Integer getActionTagCount() {
		return actionTagCount;
	}

	/**
	 * {@link #actionTagCount} mutator.
	 * @param actionTagCount	The new value.
	 **/
	@XmlElement
	public void setActionTagCount(Integer actionTagCount) {
		this.actionTagCount = actionTagCount;
	}

	/**
	 * {@link #copyToUser} accessor.
	 * @return	The value.
	 **/
	public User getCopyToUser() {
		return copyToUser;
	}

	/**
	 * {@link #copyToUser} mutator.
	 * @param copyToUser	The new value.
	 **/
	@XmlElement
	public void setCopyToUser(User copyToUser) {
		this.copyToUser = copyToUser;
	}

	/**
	 * {@link #copyToUserTagName} accessor.
	 * @return	The value.
	 **/
	public String getCopyToUserTagName() {
		return copyToUserTagName;
	}

	/**
	 * {@link #copyToUserTagName} mutator.
	 * @param copyToUserTagName	The new value.
	 **/
	@XmlElement
	public void setCopyToUserTagName(String copyToUserTagName) {
		this.copyToUserTagName = copyToUserTagName;
	}

	/**
	 * {@link #uploadModuleName} accessor.
	 * @return	The value.
	 **/
	public String getUploadModuleName() {
		return uploadModuleName;
	}

	/**
	 * {@link #uploadModuleName} mutator.
	 * @param uploadModuleName	The new value.
	 **/
	@XmlElement
	public void setUploadModuleName(String uploadModuleName) {
		preset(uploadModuleNamePropertyName, uploadModuleName);
		this.uploadModuleName = uploadModuleName;
	}

	/**
	 * {@link #uploadDocumentName} accessor.
	 * @return	The value.
	 **/
	public String getUploadDocumentName() {
		return uploadDocumentName;
	}

	/**
	 * {@link #uploadDocumentName} mutator.
	 * @param uploadDocumentName	The new value.
	 **/
	@XmlElement
	public void setUploadDocumentName(String uploadDocumentName) {
		preset(uploadDocumentNamePropertyName, uploadDocumentName);
		this.uploadDocumentName = uploadDocumentName;
	}

	/**
	 * {@link #attributeName} accessor.
	 * @return	The value.
	 **/
	public String getAttributeName() {
		return attributeName;
	}

	/**
	 * {@link #attributeName} mutator.
	 * @param attributeName	The new value.
	 **/
	@XmlElement
	public void setAttributeName(String attributeName) {
		preset(attributeNamePropertyName, attributeName);
		this.attributeName = attributeName;
	}

	/**
	 * {@link #fileHasHeaders} accessor.
	 * @return	The value.
	 **/
	public Boolean getFileHasHeaders() {
		return fileHasHeaders;
	}

	/**
	 * {@link #fileHasHeaders} mutator.
	 * @param fileHasHeaders	The new value.
	 **/
	@XmlElement
	public void setFileHasHeaders(Boolean fileHasHeaders) {
		preset(fileHasHeadersPropertyName, fileHasHeaders);
		this.fileHasHeaders = fileHasHeaders;
	}

	/**
	 * {@link #numberLoaded} accessor.
	 * @return	The value.
	 **/
	public Integer getNumberLoaded() {
		return numberLoaded;
	}

	/**
	 * {@link #numberLoaded} mutator.
	 * @param numberLoaded	The new value.
	 **/
	@XmlElement
	public void setNumberLoaded(Integer numberLoaded) {
		this.numberLoaded = numberLoaded;
	}

	/**
	 * {@link #numberMatched} accessor.
	 * @return	The value.
	 **/
	public Integer getNumberMatched() {
		return numberMatched;
	}

	/**
	 * {@link #numberMatched} mutator.
	 * @param numberMatched	The new value.
	 **/
	@XmlElement
	public void setNumberMatched(Integer numberMatched) {
		this.numberMatched = numberMatched;
	}

	/**
	 * {@link #numberTagged} accessor.
	 * @return	The value.
	 **/
	public Integer getNumberTagged() {
		return numberTagged;
	}

	/**
	 * {@link #numberTagged} mutator.
	 * @param numberTagged	The new value.
	 **/
	@XmlElement
	public void setNumberTagged(Integer numberTagged) {
		this.numberTagged = numberTagged;
	}

	/**
	 * {@link #filterOperator} accessor.
	 * @return	The value.
	 **/
	public FilterOperator getFilterOperator() {
		return filterOperator;
	}

	/**
	 * {@link #filterOperator} mutator.
	 * @param filterOperator	The new value.
	 **/
	@XmlElement
	public void setFilterOperator(FilterOperator filterOperator) {
		preset(filterOperatorPropertyName, filterOperator);
		this.filterOperator = filterOperator;
	}

	/**
	 * {@link #filterAction} accessor.
	 * @return	The value.
	 **/
	public FilterAction getFilterAction() {
		return filterAction;
	}

	/**
	 * {@link #filterAction} mutator.
	 * @param filterAction	The new value.
	 **/
	@XmlElement
	public void setFilterAction(FilterAction filterAction) {
		preset(filterActionPropertyName, filterAction);
		this.filterAction = filterAction;
	}

	/**
	 * {@link #actionModuleName} accessor.
	 * @return	The value.
	 **/
	public String getActionModuleName() {
		return actionModuleName;
	}

	/**
	 * {@link #actionModuleName} mutator.
	 * @param actionModuleName	The new value.
	 **/
	@XmlElement
	public void setActionModuleName(String actionModuleName) {
		preset(actionModuleNamePropertyName, actionModuleName);
		this.actionModuleName = actionModuleName;
	}

	/**
	 * {@link #actionDocumentName} accessor.
	 * @return	The value.
	 **/
	public String getActionDocumentName() {
		return actionDocumentName;
	}

	/**
	 * {@link #actionDocumentName} mutator.
	 * @param actionDocumentName	The new value.
	 **/
	@XmlElement
	public void setActionDocumentName(String actionDocumentName) {
		preset(actionDocumentNamePropertyName, actionDocumentName);
		this.actionDocumentName = actionDocumentName;
	}

	/**
	 * {@link #unTagSuccessful} accessor.
	 * @return	The value.
	 **/
	public Boolean getUnTagSuccessful() {
		return unTagSuccessful;
	}

	/**
	 * {@link #unTagSuccessful} mutator.
	 * @param unTagSuccessful	The new value.
	 **/
	@XmlElement
	public void setUnTagSuccessful(Boolean unTagSuccessful) {
		preset(unTagSuccessfulPropertyName, unTagSuccessful);
		this.unTagSuccessful = unTagSuccessful;
	}

	/**
	 * {@link #notification} accessor.
	 * @return	The value.
	 **/
	public Boolean getNotification() {
		return notification;
	}

	/**
	 * {@link #notification} mutator.
	 * @param notification	The new value.
	 **/
	@XmlElement
	public void setNotification(Boolean notification) {
		preset(notificationPropertyName, notification);
		this.notification = notification;
	}

	/**
	 * {@link #filterColumn} accessor.
	 * @return	The value.
	 **/
	public Integer getFilterColumn() {
		return filterColumn;
	}

	/**
	 * {@link #filterColumn} mutator.
	 * @param filterColumn	The new value.
	 **/
	@XmlElement
	public void setFilterColumn(Integer filterColumn) {
		preset(filterColumnPropertyName, filterColumn);
		this.filterColumn = filterColumn;
	}

	/**
	 * {@link #actionTag} accessor.
	 * @return	The value.
	 **/
	public Tag getActionTag() {
		return actionTag;
	}

	/**
	 * {@link #actionTag} mutator.
	 * @param actionTag	The new value.
	 **/
	@XmlElement
	public void setActionTag(Tag actionTag) {
		this.actionTag = actionTag;
	}

	/**
	 * {@link #documentCondition} accessor.
	 * @return	The value.
	 **/
	public String getDocumentCondition() {
		return documentCondition;
	}

	/**
	 * {@link #documentCondition} mutator.
	 * @param documentCondition	The new value.
	 **/
	@XmlElement
	public void setDocumentCondition(String documentCondition) {
		preset(documentConditionPropertyName, documentCondition);
		this.documentCondition = documentCondition;
	}

	/**
	 * {@link #documentAction} accessor.
	 * @return	The value.
	 **/
	public String getDocumentAction() {
		return documentAction;
	}

	/**
	 * {@link #documentAction} mutator.
	 * @param documentAction	The new value.
	 **/
	@XmlElement
	public void setDocumentAction(String documentAction) {
		preset(documentActionPropertyName, documentAction);
		this.documentAction = documentAction;
	}

	/**
	 * {@link #documentActionResults} accessor.
	 * @return	The value.
	 **/
	public String getDocumentActionResults() {
		return documentActionResults;
	}

	/**
	 * {@link #documentActionResults} mutator.
	 * @param documentActionResults	The new value.
	 **/
	@XmlElement
	public void setDocumentActionResults(String documentActionResults) {
		this.documentActionResults = documentActionResults;
	}

	/**
	 * Action Set
	 * @return	The condition

	 */
	@XmlTransient
	public boolean isActionSet() {
		return (getActionModuleName()!=null && getActionDocumentName()!=null && getDocumentAction()!=null);
	}

	/**	 * {@link #isActionSet} negation.

	 * @return	The negated condition

	 */
	public boolean isNotActionSet() {
		return (! isActionSet());
	}

	/**
	 * Attribute Set
	 * @return	The condition

	 */
	@XmlTransient
	public boolean isAttributeSet() {
		return (getUploadModuleName()!=null && getUploadDocumentName()!=null && getAttributeName()!=null);
	}

	/**	 * {@link #isAttributeSet} negation.

	 * @return	The negated condition

	 */
	public boolean isNotAttributeSet() {
		return (! isAttributeSet());
	}

	/**
	 * Explanation exists
	 * @return	The condition

	 */
	@XmlTransient
	public boolean isExplanation() {
		return (getCombinationExplanation()!=null);
	}

	/**	 * {@link #isExplanation} negation.

	 * @return	The negated condition

	 */
	public boolean isNotExplanation() {
		return (! isExplanation());
	}

	/**
	 * File Loaded
	 * @return	The condition

	 */
	@XmlTransient
	public boolean isFileLoaded() {
		return (getNumberLoaded().intValue()>0);
	}

	/**	 * {@link #isFileLoaded} negation.

	 * @return	The negated condition

	 */
	public boolean isNotFileLoaded() {
		return (! isFileLoaded());
	}
}
