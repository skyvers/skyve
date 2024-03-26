package modules.admin.domain;

import jakarta.annotation.Generated;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlEnum;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import modules.admin.Tag.TagExtension;
import modules.admin.UserProxy.UserProxyExtension;
import modules.admin.domain.DataMaintenance.EvictOption;
import org.skyve.CORE;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.types.Enumeration;
import org.skyve.impl.domain.AbstractPersistentBean;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.util.Util;

/**
 * Tag
 * 
 * @depend - - - CombinationsOperator
 * @depend - - - FilterOperator
 * @depend - - - FilterAction
 * @depend - - - EvictOption
 * @navhas n operandTag 0..1 Tag
 * @navhas n copyToUser 0..1 UserProxy
 * @stereotype "persistent"
 */
@XmlType
@XmlRootElement
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public abstract class Tag extends AbstractPersistentBean implements org.skyve.domain.app.admin.Tag {
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
	public static final String totalTaggedPropertyName = "totalTagged";

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
	public static final String uploadedPropertyName = "uploaded";

	/** @hidden */
	public static final String uploadMatchedPropertyName = "uploadMatched";

	/** @hidden */
	public static final String uploadTaggedPropertyName = "uploadTagged";

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
	public static final String operandTagPropertyName = "operandTag";

	/** @hidden */
	public static final String operandTagCountPropertyName = "operandTagCount";

	/** @hidden */
	public static final String documentConditionPropertyName = "documentCondition";

	/** @hidden */
	public static final String documentActionPropertyName = "documentAction";

	/** @hidden */
	public static final String documentActionResultsPropertyName = "documentActionResults";

	/** @hidden */
	public static final String evictOptionPropertyName = "evictOption";

	/**
	 * Operator
	 **/
	@XmlEnum
	@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
	public static enum CombinationsOperator implements Enumeration {
		union("Union", "Union"),
		except("Except", "Except"),
		intersect("Intersect", "Intersect");

		private String code;
		private String description;

		/** @hidden */
		private DomainValue domainValue;

		/** @hidden */
		private static List<DomainValue> domainValues = Stream.of(values()).map(CombinationsOperator::toDomainValue).collect(Collectors.toUnmodifiableList());

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
		public String toLocalisedDescription() {
			return Util.i18n(description);
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

		public static CombinationsOperator fromLocalisedDescription(String description) {
			CombinationsOperator result = null;

			for (CombinationsOperator value : values()) {
				if (value.toLocalisedDescription().equals(description)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static List<DomainValue> toDomainValues() {
			return domainValues;
		}
	}

	/**
	 * Filter Operator
	 **/
	@XmlEnum
	@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
	public static enum FilterOperator implements Enumeration {
		equals("equals", "Equals"),
		like("like", "Like"),
		contains("contains", "Contains");

		private String code;
		private String description;

		/** @hidden */
		private DomainValue domainValue;

		/** @hidden */
		private static List<DomainValue> domainValues = Stream.of(values()).map(FilterOperator::toDomainValue).collect(Collectors.toUnmodifiableList());

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
		public String toLocalisedDescription() {
			return Util.i18n(description);
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

		public static FilterOperator fromLocalisedDescription(String description) {
			FilterOperator result = null;

			for (FilterOperator value : values()) {
				if (value.toLocalisedDescription().equals(description)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static List<DomainValue> toDomainValues() {
			return domainValues;
		}
	}

	/**
	 * Filter Action
	 **/
	@XmlEnum
	@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
	public static enum FilterAction implements Enumeration {
		tagRecordsThatMatch("tag", "Tag records that match"),
		unTagRecordsThatMatch("unTag", "UnTag records that match");

		private String code;
		private String description;

		/** @hidden */
		private DomainValue domainValue;

		/** @hidden */
		private static List<DomainValue> domainValues = Stream.of(values()).map(FilterAction::toDomainValue).collect(Collectors.toUnmodifiableList());

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
		public String toLocalisedDescription() {
			return Util.i18n(description);
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

		public static FilterAction fromLocalisedDescription(String description) {
			FilterAction result = null;

			for (FilterAction value : values()) {
				if (value.toLocalisedDescription().equals(description)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static List<DomainValue> toDomainValues() {
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
	 * Total Tagged
	 **/
	private Long totalTagged;

	/**
	 * Copy to user
	 **/
	private UserProxyExtension copyToUser = null;

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
	 * Document
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
	 * Uploaded
	 **/
	private Long uploaded;

	/**
	 * Match
	 **/
	private Long uploadMatched;

	/**
	 * Tagged
	 **/
	private Long uploadTagged;

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
	 * Column (first column is 1)
	 **/
	private Integer filterColumn;

	/**
	 * Other Tag
	 * <br/>
	 * The other tag to use for the combination.
	 **/
	private TagExtension operandTag = null;

	/**
	 * Tagged
	 **/
	private Long operandTagCount;

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

	/**
	 * Cache Evict
	 * <br/>
	 * <p>Whether to evict each bean after processing.</p>
<p>Evicting beans will free memory for large data jobs, however there may be impacts if the action (processing) selected affects items that other beans may reference.</p>
	 **/
	private EvictOption evictOption = EvictOption.bean;

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

	public static TagExtension newInstance() {
		try {
			return CORE.getUser().getCustomer().getModule(MODULE_NAME).getDocument(CORE.getUser().getCustomer(), DOCUMENT_NAME).newInstance(CORE.getUser());
		}
		catch (RuntimeException e) {
			throw e;
		}
		catch (Exception e) {
			throw new DomainException(e);
		}
	}

	@Override
	@XmlTransient
	public String getBizKey() {
		try {
			return org.skyve.util.Binder.formatMessage("Tag {name}", this);
		}
		catch (@SuppressWarnings("unused") Exception e) {
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
	 * {@link #totalTagged} accessor.
	 * @return	The value.
	 **/
	public Long getTotalTagged() {
		return totalTagged;
	}

	/**
	 * {@link #totalTagged} mutator.
	 * @param totalTagged	The new value.
	 **/
	@XmlElement
	public void setTotalTagged(Long totalTagged) {
		this.totalTagged = totalTagged;
	}

	/**
	 * {@link #copyToUser} accessor.
	 * @return	The value.
	 **/
	public UserProxyExtension getCopyToUser() {
		return copyToUser;
	}

	/**
	 * {@link #copyToUser} mutator.
	 * @param copyToUser	The new value.
	 **/
	@XmlElement
	public void setCopyToUser(UserProxyExtension copyToUser) {
		if (this.copyToUser != copyToUser) {
			this.copyToUser = copyToUser;
		}
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
	 * {@link #uploaded} accessor.
	 * @return	The value.
	 **/
	public Long getUploaded() {
		return uploaded;
	}

	/**
	 * {@link #uploaded} mutator.
	 * @param uploaded	The new value.
	 **/
	@XmlElement
	public void setUploaded(Long uploaded) {
		this.uploaded = uploaded;
	}

	/**
	 * {@link #uploadMatched} accessor.
	 * @return	The value.
	 **/
	public Long getUploadMatched() {
		return uploadMatched;
	}

	/**
	 * {@link #uploadMatched} mutator.
	 * @param uploadMatched	The new value.
	 **/
	@XmlElement
	public void setUploadMatched(Long uploadMatched) {
		this.uploadMatched = uploadMatched;
	}

	/**
	 * {@link #uploadTagged} accessor.
	 * @return	The value.
	 **/
	public Long getUploadTagged() {
		return uploadTagged;
	}

	/**
	 * {@link #uploadTagged} mutator.
	 * @param uploadTagged	The new value.
	 **/
	@XmlElement
	public void setUploadTagged(Long uploadTagged) {
		this.uploadTagged = uploadTagged;
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
	 * {@link #operandTag} accessor.
	 * @return	The value.
	 **/
	public TagExtension getOperandTag() {
		return operandTag;
	}

	/**
	 * {@link #operandTag} mutator.
	 * @param operandTag	The new value.
	 **/
	@XmlElement
	public void setOperandTag(TagExtension operandTag) {
		if (this.operandTag != operandTag) {
			this.operandTag = operandTag;
		}
	}

	/**
	 * {@link #operandTagCount} accessor.
	 * @return	The value.
	 **/
	public Long getOperandTagCount() {
		return operandTagCount;
	}

	/**
	 * {@link #operandTagCount} mutator.
	 * @param operandTagCount	The new value.
	 **/
	@XmlElement
	public void setOperandTagCount(Long operandTagCount) {
		this.operandTagCount = operandTagCount;
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
	 * {@link #evictOption} accessor.
	 * @return	The value.
	 **/
	public EvictOption getEvictOption() {
		return evictOption;
	}

	/**
	 * {@link #evictOption} mutator.
	 * @param evictOption	The new value.
	 **/
	@XmlElement
	public void setEvictOption(EvictOption evictOption) {
		this.evictOption = evictOption;
	}

	/**
	 * Action Set
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isActionSet() {
		return (getActionModuleName()!=null && getActionDocumentName()!=null && getDocumentAction()!=null);
	}

	/**
	 * {@link #isActionSet} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotActionSet() {
		return (! isActionSet());
	}

	/**
	 * Whether to allow access to the PerformCombination action button
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isAllowCombination() {
		return (combinationsOperator!=null && operandTag!=null);
	}

	/**
	 * {@link #isAllowCombination} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotAllowCombination() {
		return (! isAllowCombination());
	}

	/**
	 * Attribute Set
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isAttributeSet() {
		return (getUploadModuleName()!=null && getUploadDocumentName()!=null && getAttributeName()!=null);
	}

	/**
	 * {@link #isAttributeSet} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotAttributeSet() {
		return (! isAttributeSet());
	}

	/**
	 * Whether to show the explanation
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isShowExplanation() {
		return (getCombinationExplanation()!=null && getCombinationExplanation().trim().length()>0);
	}

	/**
	 * {@link #isShowExplanation} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotShowExplanation() {
		return (! isShowExplanation());
	}
}
