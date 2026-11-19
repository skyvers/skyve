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
 * Dialogs Fixture
 * <br/>
 * Kitchen Sink fixture for server-side questions (EXT.getDialogs()).

			Its actions, Bizlet and view ask a question from each kind of request that can ask:
			custom actions, OK and Save (preSave), Delete (preDelete), a rerender from a field change,
			a server action in a change handler, and ZoomOut (and, in PrimeFaces, Remove) of a row.
			Outcome records what the code did after the questions were answered.
 * 
 * @navcomposed 1 rows 0..n DialogsFixtureRow
 * @stereotype "persistent"
 */
@XmlType
@XmlRootElement
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public class DialogsFixture extends AbstractPersistentBean {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "kitchensink";

	/** @hidden */
	public static final String DOCUMENT_NAME = "DialogsFixture";

	/** @hidden */
	public static final String textPropertyName = "text";

	/** @hidden */
	public static final String actionTextPropertyName = "actionText";

	/** @hidden */
	public static final String askOnSavePropertyName = "askOnSave";

	/** @hidden */
	public static final String askOnDeletePropertyName = "askOnDelete";

	/** @hidden */
	public static final String outcomePropertyName = "outcome";

	/** @hidden */
	public static final String rowsPropertyName = "rows";

	/**
	 * Text
	 * <br/>
	 * Changing it rerenders the view, and the rerender asks whether to carry on with the change logic.
	 **/
	private String text;

	/**
	 * Action Text
	 * <br/>
	 * Changing it runs the Ask Once action from its change handler.
	 **/
	private String actionText;

	/**
	 * Ask On Save
	 * <br/>
	 * When ticked, preSave asks whether to save.
	 **/
	private Boolean askOnSave = Boolean.valueOf(false);

	/**
	 * Ask On Delete
	 * <br/>
	 * When ticked, preDelete asks whether to delete.
	 **/
	private Boolean askOnDelete = Boolean.valueOf(false);

	/**
	 * Outcome
	 * <br/>
	 * What the code did after its questions were answered.
	 **/
	private String outcome;

	/**
	 * Rows
	 * <br/>
	 * Zooming out of a row asks first, as does removing a row in PrimeFaces.
	 **/
	private List<DialogsFixtureRow> rows = new ChangeTrackingArrayList<>("rows", this);

	@Override
	@XmlTransient
	public String getBizModule() {
		return DialogsFixture.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return DialogsFixture.DOCUMENT_NAME;
	}

	public static DialogsFixture newInstance() {
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
			return org.skyve.util.Binder.formatMessage("Dialogs Fixture", this);
		}
		catch (@SuppressWarnings("unused") Exception e) {
			return "Unknown";
		}
	}

	/**
	 * {@link #text} accessor.
	 * @return	The value.
	 **/
	public String getText() {
		return text;
	}

	/**
	 * {@link #text} mutator.
	 * @param text	The new value.
	 **/
	@XmlElement
	public void setText(String text) {
		preset(textPropertyName, text);
		this.text = text;
	}

	/**
	 * {@link #actionText} accessor.
	 * @return	The value.
	 **/
	public String getActionText() {
		return actionText;
	}

	/**
	 * {@link #actionText} mutator.
	 * @param actionText	The new value.
	 **/
	@XmlElement
	public void setActionText(String actionText) {
		preset(actionTextPropertyName, actionText);
		this.actionText = actionText;
	}

	/**
	 * {@link #askOnSave} accessor.
	 * @return	The value.
	 **/
	public Boolean getAskOnSave() {
		return askOnSave;
	}

	/**
	 * {@link #askOnSave} mutator.
	 * @param askOnSave	The new value.
	 **/
	@XmlElement
	public void setAskOnSave(Boolean askOnSave) {
		preset(askOnSavePropertyName, askOnSave);
		this.askOnSave = askOnSave;
	}

	/**
	 * {@link #askOnDelete} accessor.
	 * @return	The value.
	 **/
	public Boolean getAskOnDelete() {
		return askOnDelete;
	}

	/**
	 * {@link #askOnDelete} mutator.
	 * @param askOnDelete	The new value.
	 **/
	@XmlElement
	public void setAskOnDelete(Boolean askOnDelete) {
		preset(askOnDeletePropertyName, askOnDelete);
		this.askOnDelete = askOnDelete;
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
	public List<DialogsFixtureRow> getRows() {
		return rows;
	}

	/**
	 * {@link #rows} accessor.
	 * @param bizId	The bizId of the element in the list.
	 * @return	The value of the element in the list.
	 **/
	public DialogsFixtureRow getRowsElementById(String bizId) {
		return getElementById(rows, bizId);
	}

	/**
	 * {@link #rows} mutator.
	 * @param bizId	The bizId of the element in the list.
	 * @param element	The new value of the element in the list.
	 **/
	public void setRowsElementById(String bizId, DialogsFixtureRow element) {
		setElementById(rows, element);
	}

	/**
	 * {@link #rows} add.
	 * @param element	The element to add.
	 **/
	public boolean addRowsElement(DialogsFixtureRow element) {
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
	public void addRowsElement(int index, DialogsFixtureRow element) {
		rows.add(index, element);
		element.setParent(this);
	}

	/**
	 * {@link #rows} remove.
	 * @param element	The element to remove.
	 **/
	public boolean removeRowsElement(DialogsFixtureRow element) {
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
	public DialogsFixtureRow removeRowsElement(int index) {
		DialogsFixtureRow result = rows.remove(index);
		result.setParent(null);
		return result;
	}
}
