package modules.kitchensink.domain;

import jakarta.annotation.Generated;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;
import org.skyve.CORE;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.domain.AbstractTransientBean;

/**
 * List Grid Fixture
 * <br/>
 * Non-persistent Kitchen Sink fixture for the PrimeFaces list grid renderers.

			Its view lists the lookup values twice: once in a Vue list grid and once in a PrimeFaces data table,
			chosen by each list grid's componentBuilderClass property. Each grid has a selectedIdBinding, so a row
			can be selected as well as zoomed into.
 * 
 * @stereotype "transient"
 */
@XmlType
@XmlRootElement
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public class ListGridFixture extends AbstractTransientBean {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "kitchensink";

	/** @hidden */
	public static final String DOCUMENT_NAME = "ListGridFixture";

	/** @hidden */
	public static final String selectedVueIdPropertyName = "selectedVueId";

	/** @hidden */
	public static final String selectedFacesIdPropertyName = "selectedFacesId";

	/**
	 * Selected Vue Id
	 * <br/>
	 * The bizId of the row selected in the Vue list grid.
	 **/
	private String selectedVueId;

	/**
	 * Selected Faces Id
	 * <br/>
	 * The bizId of the row selected in the PrimeFaces data table.
	 **/
	private String selectedFacesId;

	@Override
	@XmlTransient
	public String getBizModule() {
		return ListGridFixture.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return ListGridFixture.DOCUMENT_NAME;
	}

	public static ListGridFixture newInstance() {
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
			return org.skyve.util.Binder.formatMessage("List Grid Fixture", this);
		}
		catch (@SuppressWarnings("unused") Exception e) {
			return "Unknown";
		}
	}

	/**
	 * {@link #selectedVueId} accessor.
	 * @return	The value.
	 **/
	public String getSelectedVueId() {
		return selectedVueId;
	}

	/**
	 * {@link #selectedVueId} mutator.
	 * @param selectedVueId	The new value.
	 **/
	@XmlElement
	public void setSelectedVueId(String selectedVueId) {
		preset(selectedVueIdPropertyName, selectedVueId);
		this.selectedVueId = selectedVueId;
	}

	/**
	 * {@link #selectedFacesId} accessor.
	 * @return	The value.
	 **/
	public String getSelectedFacesId() {
		return selectedFacesId;
	}

	/**
	 * {@link #selectedFacesId} mutator.
	 * @param selectedFacesId	The new value.
	 **/
	@XmlElement
	public void setSelectedFacesId(String selectedFacesId) {
		preset(selectedFacesIdPropertyName, selectedFacesId);
		this.selectedFacesId = selectedFacesId;
	}
}
