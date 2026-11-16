package modules.kitchensink.domain;

import jakarta.annotation.Generated;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;
import java.util.List;
import org.skyve.CORE;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.domain.AbstractTransientBean;
import org.skyve.impl.domain.ChangeTrackingArrayList;

/**
 * List Grid Fixture
 * <br/>
 * Non-persistent Kitchen Sink fixture for the PrimeFaces list grid renderers.

			Its view lists the lookup values twice: once in a Vue list grid and once in a PrimeFaces data table,
			chosen by each list grid's componentBuilderClass property. Each grid has a selectedIdBinding, so a row
			can be selected as well as zoomed into. A data grid of rows has one too, for dataGridSelect.
 * 
 * @navhas n rows 0..n LookupDescription
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

	/** @hidden */
	public static final String selectedRowIdPropertyName = "selectedRowId";

	/** @hidden */
	public static final String rowsPropertyName = "rows";

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

	/**
	 * Selected Row Id
	 * <br/>
	 * The bizId of the row selected in the data grid of rows.
	 **/
	private String selectedRowId;

	/**
	 * Rows
	 * <br/>
	 * Lookup values added to the data grid that a row can be selected in (dataGridSelect).
	 **/
	private List<LookupDescription> rows = new ChangeTrackingArrayList<>("rows", this);

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

	/**
	 * {@link #selectedRowId} accessor.
	 * @return	The value.
	 **/
	public String getSelectedRowId() {
		return selectedRowId;
	}

	/**
	 * {@link #selectedRowId} mutator.
	 * @param selectedRowId	The new value.
	 **/
	@XmlElement
	public void setSelectedRowId(String selectedRowId) {
		preset(selectedRowIdPropertyName, selectedRowId);
		this.selectedRowId = selectedRowId;
	}

	/**
	 * {@link #rows} accessor.
	 * @return	The value.
	 **/
	@XmlElement
	public List<LookupDescription> getRows() {
		return rows;
	}

	/**
	 * {@link #rows} accessor.
	 * @param bizId	The bizId of the element in the list.
	 * @return	The value of the element in the list.
	 **/
	public LookupDescription getRowsElementById(String bizId) {
		return getElementById(rows, bizId);
	}

	/**
	 * {@link #rows} mutator.
	 * @param bizId	The bizId of the element in the list.
	 * @param element	The new value of the element in the list.
	 **/
	public void setRowsElementById(String bizId, LookupDescription element) {
		setElementById(rows, element);
	}

	/**
	 * {@link #rows} add.
	 * @param element	The element to add.
	 **/
	public boolean addRowsElement(LookupDescription element) {
		return rows.add(element);
	}

	/**
	 * {@link #rows} add.
	 * @param index	The index in the list to add the element to.
	 * @param element	The element to add.
	 **/
	public void addRowsElement(int index, LookupDescription element) {
		rows.add(index, element);
	}

	/**
	 * {@link #rows} remove.
	 * @param element	The element to remove.
	 **/
	public boolean removeRowsElement(LookupDescription element) {
		return rows.remove(element);
	}

	/**
	 * {@link #rows} remove.
	 * @param index	The index in the list to remove the element from.
	 **/
	public LookupDescription removeRowsElement(int index) {
		return rows.remove(index);
	}
}
