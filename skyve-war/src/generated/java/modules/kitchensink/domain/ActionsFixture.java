package modules.kitchensink.domain;

import jakarta.annotation.Generated;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;
import java.util.List;
import org.skyve.CORE;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.domain.AbstractPersistentBean;
import org.skyve.impl.domain.ChangeTrackingArrayList;

/**
 * Actions Fixture
 * <br/>
 * Kitchen Sink fixture for the server-side refusal of actions that the view offers no reachable trigger for.

			Its edit view has one of each kind of trigger: a toolbar action, a server action in a change handler,
			a button in a form, a button in a tab, two named actions sharing a class, OK, Save and Delete,
			a link in a data grid row, and a Download, BizExport, BizImport and Upload action.
			Each Disable or Hide checkbox makes one trigger unavailable, and an action with no trigger is never available.
			Outcome records which action ran last.
 * 
 * @navcomposed 1 rows 0..n ActionsFixtureRow
 * @stereotype "persistent"
 */
@XmlType
@XmlRootElement
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public class ActionsFixture extends AbstractPersistentBean {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "kitchensink";

	/** @hidden */
	public static final String DOCUMENT_NAME = "ActionsFixture";

	/** @hidden */
	public static final String disableToolbarActionPropertyName = "disableToolbarAction";

	/** @hidden */
	public static final String eventTextPropertyName = "eventText";

	/** @hidden */
	public static final String hideEventTextPropertyName = "hideEventText";

	/** @hidden */
	public static final String disableFormPropertyName = "disableForm";

	/** @hidden */
	public static final String disableTabPropertyName = "disableTab";

	/** @hidden */
	public static final String disableSharedFirstPropertyName = "disableSharedFirst";

	/** @hidden */
	public static final String disableSharedSecondPropertyName = "disableSharedSecond";

	/** @hidden */
	public static final String disableSavePropertyName = "disableSave";

	/** @hidden */
	public static final String disableDeletePropertyName = "disableDelete";

	/** @hidden */
	public static final String downloadedPropertyName = "downloaded";

	/** @hidden */
	public static final String disableExportPropertyName = "disableExport";

	/** @hidden */
	public static final String disableImportPropertyName = "disableImport";

	/** @hidden */
	public static final String disableUploadPropertyName = "disableUpload";

	/** @hidden */
	public static final String outcomePropertyName = "outcome";

	/** @hidden */
	public static final String rowsPropertyName = "rows";

	/**
	 * Disable Toolbar Action
	 * <br/>
	 * Disables the Run From Toolbar action.
	 **/
	private Boolean disableToolbarAction = Boolean.valueOf(false);

	/**
	 * Event Text
	 * <br/>
	 * Changing it runs the Run From Event action from its change handler; nothing else triggers that action.
	 **/
	private String eventText;

	/**
	 * Hide Event Text
	 * <br/>
	 * Hides Event Text, and so the change handler that is the only trigger of Run From Event.
	 **/
	private Boolean hideEventText = Boolean.valueOf(false);

	/**
	 * Disable Form
	 * <br/>
	 * Disables the form holding the only button for Run From Form Button.
	 **/
	private Boolean disableForm = Boolean.valueOf(false);

	/**
	 * Disable Tab
	 * <br/>
	 * Disables the tab holding the only button for Run From Tab Button.
	 **/
	private Boolean disableTab = Boolean.valueOf(false);

	/**
	 * Disable Shared First
	 * <br/>
	 * Disables Run Shared First, one of the two actions with the class RunShared.
	 **/
	private Boolean disableSharedFirst = Boolean.valueOf(false);

	/**
	 * Disable Shared Second
	 * <br/>
	 * Disables Run Shared Second, the other action with the class RunShared.
	 **/
	private Boolean disableSharedSecond = Boolean.valueOf(false);

	/**
	 * Disable Save
	 * <br/>
	 * Disables OK and Save.
	 **/
	private Boolean disableSave = Boolean.valueOf(false);

	/**
	 * Disable Delete
	 * <br/>
	 * Disables Delete.
	 **/
	private Boolean disableDelete = Boolean.valueOf(false);

	/**
	 * Downloaded
	 * <br/>
	 * Set by Download Once as it prepares its download, which disables Download Once. Clear it to download again.
	 **/
	private Boolean downloaded = Boolean.valueOf(false);

	/**
	 * Disable Export
	 * <br/>
	 * Disables the Export action.
	 **/
	private Boolean disableExport = Boolean.valueOf(false);

	/**
	 * Disable Import
	 * <br/>
	 * Disables the Import action.
	 **/
	private Boolean disableImport = Boolean.valueOf(false);

	/**
	 * Disable Upload
	 * <br/>
	 * Disables the Upload action.
	 **/
	private Boolean disableUpload = Boolean.valueOf(false);

	/**
	 * Outcome
	 * <br/>
	 * Which action ran last.
	 **/
	private String outcome;

	/**
	 * Rows
	 * <br/>
	 * Each row has a Run From Row link, which the row can hide.
	 **/
	private List<ActionsFixtureRow> rows = new ChangeTrackingArrayList<>("rows", this);

	@Override
	@XmlTransient
	public String getBizModule() {
		return ActionsFixture.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return ActionsFixture.DOCUMENT_NAME;
	}

	public static ActionsFixture newInstance() {
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
			return org.skyve.util.Binder.formatMessage("Actions Fixture", this);
		}
		catch (@SuppressWarnings("unused") Exception e) {
			return "Unknown";
		}
	}

	/**
	 * {@link #disableToolbarAction} accessor.
	 * @return	The value.
	 **/
	public Boolean getDisableToolbarAction() {
		return disableToolbarAction;
	}

	/**
	 * {@link #disableToolbarAction} mutator.
	 * @param disableToolbarAction	The new value.
	 **/
	@XmlElement
	public void setDisableToolbarAction(Boolean disableToolbarAction) {
		preset(disableToolbarActionPropertyName, disableToolbarAction);
		this.disableToolbarAction = disableToolbarAction;
	}

	/**
	 * {@link #eventText} accessor.
	 * @return	The value.
	 **/
	public String getEventText() {
		return eventText;
	}

	/**
	 * {@link #eventText} mutator.
	 * @param eventText	The new value.
	 **/
	@XmlElement
	public void setEventText(String eventText) {
		preset(eventTextPropertyName, eventText);
		this.eventText = eventText;
	}

	/**
	 * {@link #hideEventText} accessor.
	 * @return	The value.
	 **/
	public Boolean getHideEventText() {
		return hideEventText;
	}

	/**
	 * {@link #hideEventText} mutator.
	 * @param hideEventText	The new value.
	 **/
	@XmlElement
	public void setHideEventText(Boolean hideEventText) {
		preset(hideEventTextPropertyName, hideEventText);
		this.hideEventText = hideEventText;
	}

	/**
	 * {@link #disableForm} accessor.
	 * @return	The value.
	 **/
	public Boolean getDisableForm() {
		return disableForm;
	}

	/**
	 * {@link #disableForm} mutator.
	 * @param disableForm	The new value.
	 **/
	@XmlElement
	public void setDisableForm(Boolean disableForm) {
		preset(disableFormPropertyName, disableForm);
		this.disableForm = disableForm;
	}

	/**
	 * {@link #disableTab} accessor.
	 * @return	The value.
	 **/
	public Boolean getDisableTab() {
		return disableTab;
	}

	/**
	 * {@link #disableTab} mutator.
	 * @param disableTab	The new value.
	 **/
	@XmlElement
	public void setDisableTab(Boolean disableTab) {
		preset(disableTabPropertyName, disableTab);
		this.disableTab = disableTab;
	}

	/**
	 * {@link #disableSharedFirst} accessor.
	 * @return	The value.
	 **/
	public Boolean getDisableSharedFirst() {
		return disableSharedFirst;
	}

	/**
	 * {@link #disableSharedFirst} mutator.
	 * @param disableSharedFirst	The new value.
	 **/
	@XmlElement
	public void setDisableSharedFirst(Boolean disableSharedFirst) {
		preset(disableSharedFirstPropertyName, disableSharedFirst);
		this.disableSharedFirst = disableSharedFirst;
	}

	/**
	 * {@link #disableSharedSecond} accessor.
	 * @return	The value.
	 **/
	public Boolean getDisableSharedSecond() {
		return disableSharedSecond;
	}

	/**
	 * {@link #disableSharedSecond} mutator.
	 * @param disableSharedSecond	The new value.
	 **/
	@XmlElement
	public void setDisableSharedSecond(Boolean disableSharedSecond) {
		preset(disableSharedSecondPropertyName, disableSharedSecond);
		this.disableSharedSecond = disableSharedSecond;
	}

	/**
	 * {@link #disableSave} accessor.
	 * @return	The value.
	 **/
	public Boolean getDisableSave() {
		return disableSave;
	}

	/**
	 * {@link #disableSave} mutator.
	 * @param disableSave	The new value.
	 **/
	@XmlElement
	public void setDisableSave(Boolean disableSave) {
		preset(disableSavePropertyName, disableSave);
		this.disableSave = disableSave;
	}

	/**
	 * {@link #disableDelete} accessor.
	 * @return	The value.
	 **/
	public Boolean getDisableDelete() {
		return disableDelete;
	}

	/**
	 * {@link #disableDelete} mutator.
	 * @param disableDelete	The new value.
	 **/
	@XmlElement
	public void setDisableDelete(Boolean disableDelete) {
		preset(disableDeletePropertyName, disableDelete);
		this.disableDelete = disableDelete;
	}

	/**
	 * {@link #downloaded} accessor.
	 * @return	The value.
	 **/
	public Boolean getDownloaded() {
		return downloaded;
	}

	/**
	 * {@link #downloaded} mutator.
	 * @param downloaded	The new value.
	 **/
	@XmlElement
	public void setDownloaded(Boolean downloaded) {
		preset(downloadedPropertyName, downloaded);
		this.downloaded = downloaded;
	}

	/**
	 * {@link #disableExport} accessor.
	 * @return	The value.
	 **/
	public Boolean getDisableExport() {
		return disableExport;
	}

	/**
	 * {@link #disableExport} mutator.
	 * @param disableExport	The new value.
	 **/
	@XmlElement
	public void setDisableExport(Boolean disableExport) {
		preset(disableExportPropertyName, disableExport);
		this.disableExport = disableExport;
	}

	/**
	 * {@link #disableImport} accessor.
	 * @return	The value.
	 **/
	public Boolean getDisableImport() {
		return disableImport;
	}

	/**
	 * {@link #disableImport} mutator.
	 * @param disableImport	The new value.
	 **/
	@XmlElement
	public void setDisableImport(Boolean disableImport) {
		preset(disableImportPropertyName, disableImport);
		this.disableImport = disableImport;
	}

	/**
	 * {@link #disableUpload} accessor.
	 * @return	The value.
	 **/
	public Boolean getDisableUpload() {
		return disableUpload;
	}

	/**
	 * {@link #disableUpload} mutator.
	 * @param disableUpload	The new value.
	 **/
	@XmlElement
	public void setDisableUpload(Boolean disableUpload) {
		preset(disableUploadPropertyName, disableUpload);
		this.disableUpload = disableUpload;
	}

	/**
	 * {@link #outcome} accessor.
	 * @return	The value.
	 **/
	public String getOutcome() {
		return outcome;
	}

	/**
	 * {@link #outcome} mutator.
	 * @param outcome	The new value.
	 **/
	@XmlElement
	public void setOutcome(String outcome) {
		preset(outcomePropertyName, outcome);
		this.outcome = outcome;
	}

	/**
	 * {@link #rows} accessor.
	 * @return	The value.
	 **/
	@XmlElement
	public List<ActionsFixtureRow> getRows() {
		return rows;
	}

	/**
	 * {@link #rows} accessor.
	 * @param bizId	The bizId of the element in the list.
	 * @return	The value of the element in the list.
	 **/
	public ActionsFixtureRow getRowsElementById(String bizId) {
		return getElementById(rows, bizId);
	}

	/**
	 * {@link #rows} mutator.
	 * @param bizId	The bizId of the element in the list.
	 * @param element	The new value of the element in the list.
	 **/
	public void setRowsElementById(String bizId, ActionsFixtureRow element) {
		setElementById(rows, element);
	}

	/**
	 * {@link #rows} add.
	 * @param element	The element to add.
	 **/
	public boolean addRowsElement(ActionsFixtureRow element) {
		boolean result = rows.add(element);
		if (result) {
			element.setParent(this);
		}
		return result;
	}

	/**
	 * {@link #rows} add.
	 * @param index	The index in the list to add the element to.
	 * @param element	The element to add.
	 **/
	public void addRowsElement(int index, ActionsFixtureRow element) {
		rows.add(index, element);
		element.setParent(this);
	}

	/**
	 * {@link #rows} remove.
	 * @param element	The element to remove.
	 **/
	public boolean removeRowsElement(ActionsFixtureRow element) {
		boolean result = rows.remove(element);
		if (result) {
			element.setParent(null);
		}
		return result;
	}

	/**
	 * {@link #rows} remove.
	 * @param index	The index in the list to remove the element from.
	 **/
	public ActionsFixtureRow removeRowsElement(int index) {
		ActionsFixtureRow result = rows.remove(index);
		result.setParent(null);
		return result;
	}

	/**
	 * Downloaded is set.
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isAlreadyDownloaded() {
		return (Boolean.TRUE.equals(getDownloaded()));
	}

	/**
	 * {@link #isAlreadyDownloaded} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotAlreadyDownloaded() {
		return (! isAlreadyDownloaded());
	}

	/**
	 * Disable Delete is set.
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isDeleteDisabled() {
		return (Boolean.TRUE.equals(getDisableDelete()));
	}

	/**
	 * {@link #isDeleteDisabled} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotDeleteDisabled() {
		return (! isDeleteDisabled());
	}

	/**
	 * Hide Event Text is set.
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isEventTextHidden() {
		return (Boolean.TRUE.equals(getHideEventText()));
	}

	/**
	 * {@link #isEventTextHidden} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotEventTextHidden() {
		return (! isEventTextHidden());
	}

	/**
	 * Disable Export is set.
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isExportDisabled() {
		return (Boolean.TRUE.equals(getDisableExport()));
	}

	/**
	 * {@link #isExportDisabled} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotExportDisabled() {
		return (! isExportDisabled());
	}

	/**
	 * Disable Form is set.
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isFormDisabled() {
		return (Boolean.TRUE.equals(getDisableForm()));
	}

	/**
	 * {@link #isFormDisabled} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotFormDisabled() {
		return (! isFormDisabled());
	}

	/**
	 * Disable Import is set.
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isImportDisabled() {
		return (Boolean.TRUE.equals(getDisableImport()));
	}

	/**
	 * {@link #isImportDisabled} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotImportDisabled() {
		return (! isImportDisabled());
	}

	/**
	 * Never true for the fixture itself. The Run From Row link in the rows grid is hidden by the row's rowLinkHidden condition, which the renderers evaluate against each row, but the view validator looks up a grid link's condition in this document.
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isRowLinkHidden() {
		return (false);
	}

	/**
	 * {@link #isRowLinkHidden} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotRowLinkHidden() {
		return (! isRowLinkHidden());
	}

	/**
	 * Disable Save is set.
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isSaveDisabled() {
		return (Boolean.TRUE.equals(getDisableSave()));
	}

	/**
	 * {@link #isSaveDisabled} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotSaveDisabled() {
		return (! isSaveDisabled());
	}

	/**
	 * Disable Shared First is set.
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isSharedFirstDisabled() {
		return (Boolean.TRUE.equals(getDisableSharedFirst()));
	}

	/**
	 * {@link #isSharedFirstDisabled} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotSharedFirstDisabled() {
		return (! isSharedFirstDisabled());
	}

	/**
	 * Disable Shared Second is set.
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isSharedSecondDisabled() {
		return (Boolean.TRUE.equals(getDisableSharedSecond()));
	}

	/**
	 * {@link #isSharedSecondDisabled} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotSharedSecondDisabled() {
		return (! isSharedSecondDisabled());
	}

	/**
	 * Disable Tab is set.
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isTabDisabled() {
		return (Boolean.TRUE.equals(getDisableTab()));
	}

	/**
	 * {@link #isTabDisabled} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotTabDisabled() {
		return (! isTabDisabled());
	}

	/**
	 * Disable Toolbar Action is set.
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isToolbarActionDisabled() {
		return (Boolean.TRUE.equals(getDisableToolbarAction()));
	}

	/**
	 * {@link #isToolbarActionDisabled} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotToolbarActionDisabled() {
		return (! isToolbarActionDisabled());
	}

	/**
	 * Disable Upload is set.
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isUploadDisabled() {
		return (Boolean.TRUE.equals(getDisableUpload()));
	}

	/**
	 * {@link #isUploadDisabled} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotUploadDisabled() {
		return (! isUploadDisabled());
	}
}
