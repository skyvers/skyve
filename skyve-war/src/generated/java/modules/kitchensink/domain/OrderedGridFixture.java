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
 * Ordered Grid Fixture
 * <br/>
 * Kitchen Sink fixture for ordered collections that are saved: a data grid, an inline data grid and a list
			membership, whose rows (or members) can be dragged into a new order, kept by their ordinals.
 * 
 * @navhas n members 0..n LookupDescription
 * @navcomposed 1 rows 0..n OrderedGridFixtureRow
 * @navcomposed 1 lines 0..n OrderedGridFixtureLine
 * @stereotype "persistent"
 */
@XmlType
@XmlRootElement
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public class OrderedGridFixture extends AbstractPersistentBean {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "kitchensink";

	/** @hidden */
	public static final String DOCUMENT_NAME = "OrderedGridFixture";

	/** @hidden */
	public static final String namePropertyName = "name";

	/** @hidden */
	public static final String rowsPropertyName = "rows";

	/** @hidden */
	public static final String linesPropertyName = "lines";

	/** @hidden */
	public static final String membersPropertyName = "members";

	/**
	 * Name
	 **/
	private String name;

	/**
	 * Rows
	 * <br/>
	 * An ordered collection: its rows can be dragged into a new order.
	 **/
	private List<OrderedGridFixtureRow> rows = new ChangeTrackingArrayList<>("rows", this);

	/**
	 * Lines
	 * <br/>
	 * An ordered collection, edited inline: its rows can be dragged into a new order.
	 **/
	private List<OrderedGridFixtureLine> lines = new ChangeTrackingArrayList<>("lines", this);

	/**
	 * Members
	 * <br/>
	 * An ordered list membership of lookup values: its members can be dragged into a new order.
	 **/
	private List<LookupDescription> members = new ChangeTrackingArrayList<>("members", this);

	@Override
	@XmlTransient
	public String getBizModule() {
		return OrderedGridFixture.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return OrderedGridFixture.DOCUMENT_NAME;
	}

	public static OrderedGridFixture newInstance() {
		try {
			return KitchensinkDocument.ORDERED_GRID_FIXTURE.toDocument(CORE.getUser().getCustomer()).newInstance(CORE.getUser());
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
	 * {@link #rows} accessor.
	 * @return	The value.
	 **/
	@XmlElement
	public List<OrderedGridFixtureRow> getRows() {
		return rows;
	}

	/**
	 * {@link #rows} accessor.
	 * @param bizId	The bizId of the element in the list.
	 * @return	The value of the element in the list.
	 **/
	public OrderedGridFixtureRow getRowsElementById(String bizId) {
		return getElementById(rows, bizId);
	}

	/**
	 * {@link #rows} mutator.
	 * @param bizId	The bizId of the element in the list.
	 * @param element	The new value of the element in the list.
	 **/
	public void setRowsElementById(String bizId, OrderedGridFixtureRow element) {
		setElementById(rows, element);
	}

	/**
	 * {@link #rows} add.
	 * @param element	The element to add.
	 **/
	public boolean addRowsElement(OrderedGridFixtureRow element) {
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
	public void addRowsElement(int index, OrderedGridFixtureRow element) {
		rows.add(index, element);
		element.setParent(this);
	}

	/**
	 * {@link #rows} remove.
	 * @param element	The element to remove.
	 **/
	public boolean removeRowsElement(OrderedGridFixtureRow element) {
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
	public OrderedGridFixtureRow removeRowsElement(int index) {
		OrderedGridFixtureRow result = rows.remove(index);
		result.setParent(null);
		return result;
	}

	/**
	 * {@link #lines} accessor.
	 * @return	The value.
	 **/
	@XmlElement
	public List<OrderedGridFixtureLine> getLines() {
		return lines;
	}

	/**
	 * {@link #lines} accessor.
	 * @param bizId	The bizId of the element in the list.
	 * @return	The value of the element in the list.
	 **/
	public OrderedGridFixtureLine getLinesElementById(String bizId) {
		return getElementById(lines, bizId);
	}

	/**
	 * {@link #lines} mutator.
	 * @param bizId	The bizId of the element in the list.
	 * @param element	The new value of the element in the list.
	 **/
	public void setLinesElementById(String bizId, OrderedGridFixtureLine element) {
		setElementById(lines, element);
	}

	/**
	 * {@link #lines} add.
	 * @param element	The element to add.
	 **/
	public boolean addLinesElement(OrderedGridFixtureLine element) {
		boolean result = lines.add(element);
		if (result) {
			element.setParent(this);
		}
		return result;
	}

	/**
	 * {@link #lines} add.
	 * @param index	The index in the list to add the element to.
	 * @param element	The element to add.
	 **/
	public void addLinesElement(int index, OrderedGridFixtureLine element) {
		lines.add(index, element);
		element.setParent(this);
	}

	/**
	 * {@link #lines} remove.
	 * @param element	The element to remove.
	 **/
	public boolean removeLinesElement(OrderedGridFixtureLine element) {
		boolean result = lines.remove(element);
		if (result) {
			element.setParent(null);
		}
		return result;
	}

	/**
	 * {@link #lines} remove.
	 * @param index	The index in the list to remove the element from.
	 **/
	public OrderedGridFixtureLine removeLinesElement(int index) {
		OrderedGridFixtureLine result = lines.remove(index);
		result.setParent(null);
		return result;
	}

	/**
	 * {@link #members} accessor.
	 * @return	The value.
	 **/
	@XmlElement
	public List<LookupDescription> getMembers() {
		return members;
	}

	/**
	 * {@link #members} accessor.
	 * @param bizId	The bizId of the element in the list.
	 * @return	The value of the element in the list.
	 **/
	public LookupDescription getMembersElementById(String bizId) {
		return getElementById(members, bizId);
	}

	/**
	 * {@link #members} mutator.
	 * @param bizId	The bizId of the element in the list.
	 * @param element	The new value of the element in the list.
	 **/
	public void setMembersElementById(String bizId, LookupDescription element) {
		setElementById(members, element);
	}

	/**
	 * {@link #members} add.
	 * @param element	The element to add.
	 **/
	public boolean addMembersElement(LookupDescription element) {
		return members.add(element);
	}

	/**
	 * {@link #members} add.
	 * @param index	The index in the list to add the element to.
	 * @param element	The element to add.
	 **/
	public void addMembersElement(int index, LookupDescription element) {
		members.add(index, element);
	}

	/**
	 * {@link #members} remove.
	 * @param element	The element to remove.
	 **/
	public boolean removeMembersElement(LookupDescription element) {
		return members.remove(element);
	}

	/**
	 * {@link #members} remove.
	 * @param index	The index in the list to remove the element from.
	 **/
	public LookupDescription removeMembersElement(int index) {
		return members.remove(index);
	}
}
