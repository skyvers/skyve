package modules.test.domain;

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
 * Reachability
 * <br/>
 * Test persistence by reachability to inform when to call Bizlet events.
 * 
 * @navcomposed n nonPersistentComposedCollection 0..n AllAttributesPersistent
 * @navcomposed n persistentComposedAssociation 0..1 AllAttributesPersistent
 * @navcomposed n nonPersistentComposedAssociation 0..1 AllAttributesPersistent
 * @navhas n persistentAggregatedAssociation 0..1 AllAttributesPersistent
 * @navhas n persistentAggregatedCollection 0..n AllAttributesPersistent
 * @navcomposed n persistentComposedCollection 0..n AllAttributesPersistent
 * @navhas n nonPersistentAggregatedCollection 0..n AllAttributesPersistent
 * @navhas n nonPersistentAggregatedAssociation 0..1 AllAttributesPersistent
 * @stereotype "persistent"
 */
@XmlType
@XmlRootElement
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator", date = "2024-03-25T03:04:55.000Z")
public class Reachability extends AbstractPersistentBean {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "test";

	/** @hidden */
	public static final String DOCUMENT_NAME = "Reachability";

	/** @hidden */
	public static final String textPropertyName = "text";

	/** @hidden */
	public static final String nonPersistentAggregatedAssociationPropertyName = "nonPersistentAggregatedAssociation";

	/** @hidden */
	public static final String persistentAggregatedAssociationPropertyName = "persistentAggregatedAssociation";

	/** @hidden */
	public static final String nonPersistentAggregatedCollectionPropertyName = "nonPersistentAggregatedCollection";

	/** @hidden */
	public static final String persistentAggregatedCollectionPropertyName = "persistentAggregatedCollection";

	/** @hidden */
	public static final String nonPersistentComposedAssociationPropertyName = "nonPersistentComposedAssociation";

	/** @hidden */
	public static final String persistentComposedAssociationPropertyName = "persistentComposedAssociation";

	/** @hidden */
	public static final String nonPersistentComposedCollectionPropertyName = "nonPersistentComposedCollection";

	/** @hidden */
	public static final String persistentComposedCollectionPropertyName = "persistentComposedCollection";

	/**
	 * Text
	 **/
	private String text;

	/**
	 * Non Persistent Aggregated Association
	 **/
	private AllAttributesPersistent nonPersistentAggregatedAssociation = null;

	/**
	 * Persistent Aggregated Association
	 **/
	private AllAttributesPersistent persistentAggregatedAssociation = null;

	/**
	 * Non Persistent Aggregated Collection
	 **/
	private List<AllAttributesPersistent> nonPersistentAggregatedCollection = new ChangeTrackingArrayList<>("nonPersistentAggregatedCollection", this);

	/**
	 * Persistent Aggregated Collection
	 **/
	private List<AllAttributesPersistent> persistentAggregatedCollection = new ChangeTrackingArrayList<>("persistentAggregatedCollection", this);

	/**
	 * Non Persistent Composed Association
	 **/
	private AllAttributesPersistent nonPersistentComposedAssociation = null;

	/**
	 * Persistent Composed Association
	 **/
	private AllAttributesPersistent persistentComposedAssociation = null;

	/**
	 * Non Persistent Composed Collection
	 **/
	private List<AllAttributesPersistent> nonPersistentComposedCollection = new ChangeTrackingArrayList<>("nonPersistentComposedCollection", this);

	/**
	 * Persistent Composed Collection
	 **/
	private List<AllAttributesPersistent> persistentComposedCollection = new ChangeTrackingArrayList<>("persistentComposedCollection", this);

	@Override
	@XmlTransient
	public String getBizModule() {
		return Reachability.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return Reachability.DOCUMENT_NAME;
	}

	public static Reachability newInstance() {
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
			return org.skyve.util.Binder.formatMessage("{text}", this);
		}
		catch (@SuppressWarnings("unused") Exception e) {
			return "Unknown";
		}
	}

	@Override
	public boolean equals(Object o) {
		return ((o instanceof Reachability) && 
					this.getBizId().equals(((Reachability) o).getBizId()));
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
	 * {@link #nonPersistentAggregatedAssociation} accessor.
	 * @return	The value.
	 **/
	public AllAttributesPersistent getNonPersistentAggregatedAssociation() {
		return nonPersistentAggregatedAssociation;
	}

	/**
	 * {@link #nonPersistentAggregatedAssociation} mutator.
	 * @param nonPersistentAggregatedAssociation	The new value.
	 **/
	@XmlElement
	public void setNonPersistentAggregatedAssociation(AllAttributesPersistent nonPersistentAggregatedAssociation) {
		if (this.nonPersistentAggregatedAssociation != nonPersistentAggregatedAssociation) {
			preset(nonPersistentAggregatedAssociationPropertyName, nonPersistentAggregatedAssociation);
			this.nonPersistentAggregatedAssociation = nonPersistentAggregatedAssociation;
		}
	}

	/**
	 * {@link #persistentAggregatedAssociation} accessor.
	 * @return	The value.
	 **/
	public AllAttributesPersistent getPersistentAggregatedAssociation() {
		return persistentAggregatedAssociation;
	}

	/**
	 * {@link #persistentAggregatedAssociation} mutator.
	 * @param persistentAggregatedAssociation	The new value.
	 **/
	@XmlElement
	public void setPersistentAggregatedAssociation(AllAttributesPersistent persistentAggregatedAssociation) {
		if (this.persistentAggregatedAssociation != persistentAggregatedAssociation) {
			preset(persistentAggregatedAssociationPropertyName, persistentAggregatedAssociation);
			this.persistentAggregatedAssociation = persistentAggregatedAssociation;
		}
	}

	/**
	 * {@link #nonPersistentAggregatedCollection} accessor.
	 * @return	The value.
	 **/
	@XmlElement
	public List<AllAttributesPersistent> getNonPersistentAggregatedCollection() {
		return nonPersistentAggregatedCollection;
	}

	/**
	 * {@link #nonPersistentAggregatedCollection} accessor.
	 * @param bizId	The bizId of the element in the list.
	 * @return	The value of the element in the list.
	 **/
	public AllAttributesPersistent getNonPersistentAggregatedCollectionElementById(String bizId) {
		return getElementById(nonPersistentAggregatedCollection, bizId);
	}

	/**
	 * {@link #nonPersistentAggregatedCollection} mutator.
	 * @param bizId	The bizId of the element in the list.
	 * @param element	The new value of the element in the list.
	 **/
	public void setNonPersistentAggregatedCollectionElementById(String bizId, AllAttributesPersistent element) {
		setElementById(nonPersistentAggregatedCollection, element);
	}

	/**
	 * {@link #nonPersistentAggregatedCollection} add.
	 * @param element	The element to add.
	 **/
	public boolean addNonPersistentAggregatedCollectionElement(AllAttributesPersistent element) {
		return nonPersistentAggregatedCollection.add(element);
	}

	/**
	 * {@link #nonPersistentAggregatedCollection} add.
	 * @param index	The index in the list to add the element to.
	 * @param element	The element to add.
	 **/
	public void addNonPersistentAggregatedCollectionElement(int index, AllAttributesPersistent element) {
		nonPersistentAggregatedCollection.add(index, element);
	}

	/**
	 * {@link #nonPersistentAggregatedCollection} remove.
	 * @param element	The element to remove.
	 **/
	public boolean removeNonPersistentAggregatedCollectionElement(AllAttributesPersistent element) {
		return nonPersistentAggregatedCollection.remove(element);
	}

	/**
	 * {@link #nonPersistentAggregatedCollection} remove.
	 * @param index	The index in the list to remove the element from.
	 **/
	public AllAttributesPersistent removeNonPersistentAggregatedCollectionElement(int index) {
		return nonPersistentAggregatedCollection.remove(index);
	}

	/**
	 * {@link #persistentAggregatedCollection} accessor.
	 * @return	The value.
	 **/
	@XmlElement
	public List<AllAttributesPersistent> getPersistentAggregatedCollection() {
		return persistentAggregatedCollection;
	}

	/**
	 * {@link #persistentAggregatedCollection} accessor.
	 * @param bizId	The bizId of the element in the list.
	 * @return	The value of the element in the list.
	 **/
	public AllAttributesPersistent getPersistentAggregatedCollectionElementById(String bizId) {
		return getElementById(persistentAggregatedCollection, bizId);
	}

	/**
	 * {@link #persistentAggregatedCollection} mutator.
	 * @param bizId	The bizId of the element in the list.
	 * @param element	The new value of the element in the list.
	 **/
	public void setPersistentAggregatedCollectionElementById(String bizId, AllAttributesPersistent element) {
		setElementById(persistentAggregatedCollection, element);
	}

	/**
	 * {@link #persistentAggregatedCollection} add.
	 * @param element	The element to add.
	 **/
	public boolean addPersistentAggregatedCollectionElement(AllAttributesPersistent element) {
		return persistentAggregatedCollection.add(element);
	}

	/**
	 * {@link #persistentAggregatedCollection} add.
	 * @param index	The index in the list to add the element to.
	 * @param element	The element to add.
	 **/
	public void addPersistentAggregatedCollectionElement(int index, AllAttributesPersistent element) {
		persistentAggregatedCollection.add(index, element);
	}

	/**
	 * {@link #persistentAggregatedCollection} remove.
	 * @param element	The element to remove.
	 **/
	public boolean removePersistentAggregatedCollectionElement(AllAttributesPersistent element) {
		return persistentAggregatedCollection.remove(element);
	}

	/**
	 * {@link #persistentAggregatedCollection} remove.
	 * @param index	The index in the list to remove the element from.
	 **/
	public AllAttributesPersistent removePersistentAggregatedCollectionElement(int index) {
		return persistentAggregatedCollection.remove(index);
	}

	/**
	 * {@link #nonPersistentComposedAssociation} accessor.
	 * @return	The value.
	 **/
	public AllAttributesPersistent getNonPersistentComposedAssociation() {
		return nonPersistentComposedAssociation;
	}

	/**
	 * {@link #nonPersistentComposedAssociation} mutator.
	 * @param nonPersistentComposedAssociation	The new value.
	 **/
	@XmlElement
	public void setNonPersistentComposedAssociation(AllAttributesPersistent nonPersistentComposedAssociation) {
		if (this.nonPersistentComposedAssociation != nonPersistentComposedAssociation) {
			preset(nonPersistentComposedAssociationPropertyName, nonPersistentComposedAssociation);
			this.nonPersistentComposedAssociation = nonPersistentComposedAssociation;
		}
	}

	/**
	 * {@link #persistentComposedAssociation} accessor.
	 * @return	The value.
	 **/
	public AllAttributesPersistent getPersistentComposedAssociation() {
		return persistentComposedAssociation;
	}

	/**
	 * {@link #persistentComposedAssociation} mutator.
	 * @param persistentComposedAssociation	The new value.
	 **/
	@XmlElement
	public void setPersistentComposedAssociation(AllAttributesPersistent persistentComposedAssociation) {
		if (this.persistentComposedAssociation != persistentComposedAssociation) {
			preset(persistentComposedAssociationPropertyName, persistentComposedAssociation);
			this.persistentComposedAssociation = persistentComposedAssociation;
		}
	}

	/**
	 * {@link #nonPersistentComposedCollection} accessor.
	 * @return	The value.
	 **/
	@XmlElement
	public List<AllAttributesPersistent> getNonPersistentComposedCollection() {
		return nonPersistentComposedCollection;
	}

	/**
	 * {@link #nonPersistentComposedCollection} accessor.
	 * @param bizId	The bizId of the element in the list.
	 * @return	The value of the element in the list.
	 **/
	public AllAttributesPersistent getNonPersistentComposedCollectionElementById(String bizId) {
		return getElementById(nonPersistentComposedCollection, bizId);
	}

	/**
	 * {@link #nonPersistentComposedCollection} mutator.
	 * @param bizId	The bizId of the element in the list.
	 * @param element	The new value of the element in the list.
	 **/
	public void setNonPersistentComposedCollectionElementById(String bizId, AllAttributesPersistent element) {
		setElementById(nonPersistentComposedCollection, element);
	}

	/**
	 * {@link #nonPersistentComposedCollection} add.
	 * @param element	The element to add.
	 **/
	public boolean addNonPersistentComposedCollectionElement(AllAttributesPersistent element) {
		return nonPersistentComposedCollection.add(element);
	}

	/**
	 * {@link #nonPersistentComposedCollection} add.
	 * @param index	The index in the list to add the element to.
	 * @param element	The element to add.
	 **/
	public void addNonPersistentComposedCollectionElement(int index, AllAttributesPersistent element) {
		nonPersistentComposedCollection.add(index, element);
	}

	/**
	 * {@link #nonPersistentComposedCollection} remove.
	 * @param element	The element to remove.
	 **/
	public boolean removeNonPersistentComposedCollectionElement(AllAttributesPersistent element) {
		return nonPersistentComposedCollection.remove(element);
	}

	/**
	 * {@link #nonPersistentComposedCollection} remove.
	 * @param index	The index in the list to remove the element from.
	 **/
	public AllAttributesPersistent removeNonPersistentComposedCollectionElement(int index) {
		return nonPersistentComposedCollection.remove(index);
	}

	/**
	 * {@link #persistentComposedCollection} accessor.
	 * @return	The value.
	 **/
	@XmlElement
	public List<AllAttributesPersistent> getPersistentComposedCollection() {
		return persistentComposedCollection;
	}

	/**
	 * {@link #persistentComposedCollection} accessor.
	 * @param bizId	The bizId of the element in the list.
	 * @return	The value of the element in the list.
	 **/
	public AllAttributesPersistent getPersistentComposedCollectionElementById(String bizId) {
		return getElementById(persistentComposedCollection, bizId);
	}

	/**
	 * {@link #persistentComposedCollection} mutator.
	 * @param bizId	The bizId of the element in the list.
	 * @param element	The new value of the element in the list.
	 **/
	public void setPersistentComposedCollectionElementById(String bizId, AllAttributesPersistent element) {
		setElementById(persistentComposedCollection, element);
	}

	/**
	 * {@link #persistentComposedCollection} add.
	 * @param element	The element to add.
	 **/
	public boolean addPersistentComposedCollectionElement(AllAttributesPersistent element) {
		return persistentComposedCollection.add(element);
	}

	/**
	 * {@link #persistentComposedCollection} add.
	 * @param index	The index in the list to add the element to.
	 * @param element	The element to add.
	 **/
	public void addPersistentComposedCollectionElement(int index, AllAttributesPersistent element) {
		persistentComposedCollection.add(index, element);
	}

	/**
	 * {@link #persistentComposedCollection} remove.
	 * @param element	The element to remove.
	 **/
	public boolean removePersistentComposedCollectionElement(AllAttributesPersistent element) {
		return persistentComposedCollection.remove(element);
	}

	/**
	 * {@link #persistentComposedCollection} remove.
	 * @param index	The index in the list to remove the element from.
	 **/
	public AllAttributesPersistent removePersistentComposedCollectionElement(int index) {
		return persistentComposedCollection.remove(index);
	}
}
