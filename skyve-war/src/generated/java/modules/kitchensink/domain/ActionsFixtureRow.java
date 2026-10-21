package modules.kitchensink.domain;

import jakarta.annotation.Generated;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;
import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.ChildBean;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.domain.AbstractPersistentBean;

/**
 * Actions Fixture Row
 * <br/>
 * Row of the Kitchen Sink actions fixture. The fixture's rows grid links each row to the Run From Row action,
			which this document's edit view declares out of its action panel, so the link is the action's only trigger.

			SmartClient shows the link from the link's own condition, rowLinkHidden, evaluated for the row, and ignores the action's.
			PrimeFaces shows it from the action's conditions, rowActionDisabled here, evaluated for the row, and ignores the link's.

			The edit view also offers Export From Row, so that an export requested while zoomed in on a row can be checked against the row's view.
 * 
 * @stereotype "persistent child"
 */
@XmlType
@XmlRootElement
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public class ActionsFixtureRow extends AbstractPersistentBean implements ChildBean<ActionsFixture> {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "kitchensink";

	/** @hidden */
	public static final String DOCUMENT_NAME = "ActionsFixtureRow";

	/** @hidden */
	public static final String namePropertyName = "name";

	/** @hidden */
	public static final String hideLinkPropertyName = "hideLink";

	/** @hidden */
	public static final String disableRowActionPropertyName = "disableRowAction";

	/** @hidden */
	public static final String outcomePropertyName = "outcome";

	/**
	 * Name
	 **/
	private String name;

	/**
	 * Hide Link
	 * <br/>
	 * Hides this row's Run From Row link in the fixture's rows grid in SmartClient.
	 **/
	private Boolean hideLink = Boolean.valueOf(false);

	/**
	 * Disable Row Action
	 * <br/>
	 * Disables Run From Row's link in the fixture's rows grid in PrimeFaces, which applies the row action's conditions to the link. SmartClient, and the server check, apply only the link's own condition.
	 **/
	private Boolean disableRowAction = Boolean.valueOf(false);

	/**
	 * Outcome
	 * <br/>
	 * Whether Run From Row ran for this row.
	 **/
	private String outcome;

	private ActionsFixture parent;

	private Integer bizOrdinal;

	@Override
	@XmlTransient
	public String getBizModule() {
		return ActionsFixtureRow.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return ActionsFixtureRow.DOCUMENT_NAME;
	}

	public static ActionsFixtureRow newInstance() {
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
			return org.skyve.util.Binder.formatMessage("Actions Fixture Row", this);
		}
		catch (@SuppressWarnings("unused") Exception e) {
			return "Unknown";
		}
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
	 * {@link #hideLink} accessor.
	 * @return	The value.
	 **/
	public Boolean getHideLink() {
		return hideLink;
	}

	/**
	 * {@link #hideLink} mutator.
	 * @param hideLink	The new value.
	 **/
	@XmlElement
	public void setHideLink(Boolean hideLink) {
		preset(hideLinkPropertyName, hideLink);
		this.hideLink = hideLink;
	}

	/**
	 * {@link #disableRowAction} accessor.
	 * @return	The value.
	 **/
	public Boolean getDisableRowAction() {
		return disableRowAction;
	}

	/**
	 * {@link #disableRowAction} mutator.
	 * @param disableRowAction	The new value.
	 **/
	@XmlElement
	public void setDisableRowAction(Boolean disableRowAction) {
		preset(disableRowActionPropertyName, disableRowAction);
		this.disableRowAction = disableRowAction;
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
	 * Disable Row Action is set.
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isRowActionDisabled() {
		return (Boolean.TRUE.equals(getDisableRowAction()));
	}

	/**
	 * {@link #isRowActionDisabled} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotRowActionDisabled() {
		return (! isRowActionDisabled());
	}

	/**
	 * Hide Link is set.
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isRowLinkHidden() {
		return (Boolean.TRUE.equals(getHideLink()));
	}

	/**
	 * {@link #isRowLinkHidden} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotRowLinkHidden() {
		return (! isRowLinkHidden());
	}

	@Override
	public ActionsFixture getParent() {
		return parent;
	}

	@Override
	@XmlElement
	public void setParent(ActionsFixture parent) {
		if (this.parent != parent) {
			preset(ChildBean.PARENT_NAME, parent);
			this.parent = parent;
		}
	}

	@Override
	public Integer getBizOrdinal() {
		return bizOrdinal;
	}

	@Override
	@XmlElement
	public void setBizOrdinal(Integer bizOrdinal) {
		preset(Bean.ORDINAL_NAME, bizOrdinal);
		this.bizOrdinal =  bizOrdinal;
	}
}
