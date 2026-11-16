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
 * Model Lookup Fixture
 * 
 * @navhas n selection 0..1 LookupDescription
 * @navhas n singleSelection 0..1 LookupDescription
 * @navcomposed 1 rows 0..n ModelLookupRow
 * @stereotype "persistent"
 */
@XmlType
@XmlRootElement
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public class ModelLookupFixture extends AbstractPersistentBean {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "kitchensink";

	/** @hidden */
	public static final String DOCUMENT_NAME = "ModelLookupFixture";

	/** @hidden */
	public static final String namePropertyName = "name";

	/** @hidden */
	public static final String selectionPropertyName = "selection";

	/** @hidden */
	public static final String rowsPropertyName = "rows";

	/** @hidden */
	public static final String singleSelectionPropertyName = "singleSelection";

	/**
	 * Name
	 **/
	private String name = "Model lookup example";

	/**
	 * Form Model Lookup
	 **/
	private LookupDescription selection = null;

	/**
	 * Model Lookup Rows
	 **/
	private List<ModelLookupRow> rows = new ChangeTrackingArrayList<>("rows", this);

	/**
	 * Single-column Model Lookup
	 **/
	private LookupDescription singleSelection = null;

	@Override
	@XmlTransient
	public String getBizModule() {
		return ModelLookupFixture.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return ModelLookupFixture.DOCUMENT_NAME;
	}

	public static ModelLookupFixture newInstance() {
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
			return org.skyve.util.Binder.formatMessage("{name}", this);
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
	 * {@link #selection} accessor.
	 * @return	The value.
	 **/
	public LookupDescription getSelection() {
		return selection;
	}

	/**
	 * {@link #selection} mutator.
	 * @param selection	The new value.
	 **/
	@XmlElement
	public void setSelection(LookupDescription selection) {
		if (this.selection != selection) {
			preset(selectionPropertyName, selection);
			this.selection = selection;
		}
	}

	/**
	 * {@link #rows} accessor.
	 * @return	The value.
	 **/
	@XmlElement
	public List<ModelLookupRow> getRows() {
		return rows;
	}

	/**
	 * {@link #rows} accessor.
	 * @param bizId	The bizId of the element in the list.
	 * @return	The value of the element in the list.
	 **/
	public ModelLookupRow getRowsElementById(String bizId) {
		return getElementById(rows, bizId);
	}

	/**
	 * {@link #rows} mutator.
	 * @param bizId	The bizId of the element in the list.
	 * @param element	The new value of the element in the list.
	 **/
	public void setRowsElementById(String bizId, ModelLookupRow element) {
		setElementById(rows, element);
	}

	/**
	 * {@link #rows} add.
	 * @param element	The element to add.
	 **/
	public boolean addRowsElement(ModelLookupRow element) {
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
	public void addRowsElement(int index, ModelLookupRow element) {
		rows.add(index, element);
		element.setParent(this);
	}

	/**
	 * {@link #rows} remove.
	 * @param element	The element to remove.
	 **/
	public boolean removeRowsElement(ModelLookupRow element) {
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
	public ModelLookupRow removeRowsElement(int index) {
		ModelLookupRow result = rows.remove(index);
		result.setParent(null);
		return result;
	}

	/**
	 * {@link #singleSelection} accessor.
	 * @return	The value.
	 **/
	public LookupDescription getSingleSelection() {
		return singleSelection;
	}

	/**
	 * {@link #singleSelection} mutator.
	 * @param singleSelection	The new value.
	 **/
	@XmlElement
	public void setSingleSelection(LookupDescription singleSelection) {
		if (this.singleSelection != singleSelection) {
			preset(singleSelectionPropertyName, singleSelection);
			this.singleSelection = singleSelection;
		}
	}
}
