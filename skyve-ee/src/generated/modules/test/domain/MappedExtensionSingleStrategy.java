package modules.test.domain;

import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;
import modules.test.MappedExtensionSingleStrategy.MappedExtensionSingleStrategyExtension;
import modules.test.domain.MappedBase;
import org.skyve.CORE;
import org.skyve.domain.PolymorphicPersistentBean;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.domain.ChangeTrackingArrayList;

/**
 * Mapped Extension Single Strategy
 * <br/>
 * Extension document.
 * 
 * @navhas n aggregatedCollection 0..n MappedExtensionSingleStrategy
 * @navcomposed n composedCollection 0..n MappedExtensionSingleStrategy
 * @navhas n aggregatedAssociation 0..1 MappedExtensionSingleStrategy
 * @stereotype "persistent"
 */
@XmlType
@XmlRootElement
@PolymorphicPersistentBean
public abstract class MappedExtensionSingleStrategy extends MappedBase {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "test";
	/** @hidden */
	public static final String DOCUMENT_NAME = "MappedExtensionSingleStrategy";

	/** @hidden */
	public static final String aggregatedAssociationPropertyName = "aggregatedAssociation";
	/** @hidden */
	public static final String aggregatedCollectionPropertyName = "aggregatedCollection";
	/** @hidden */
	public static final String composedCollectionPropertyName = "composedCollection";
	/** @hidden */
	public static final String inverseAggregatedAssociationPropertyName = "inverseAggregatedAssociation";
	/** @hidden */
	public static final String derivedIntegerPropertyName = "derivedInteger";

	/**
	 * Aggregated Association
	 **/
	private MappedExtensionSingleStrategyExtension aggregatedAssociation = null;
	/**
	 * Aggregated Collection
	 **/
	private List<MappedExtensionSingleStrategyExtension> aggregatedCollection = new ChangeTrackingArrayList<>("aggregatedCollection", this);
	/**
	 * Composed Collection
	 **/
	private List<MappedExtensionSingleStrategyExtension> composedCollection = new ChangeTrackingArrayList<>("composedCollection", this);
	/**
	 * Inverse
	 **/
	private List<MappedExtensionSingleStrategyExtension> inverseAggregatedAssociation = new ArrayList<>();
	/**
	 * Derived Integer
	 **/
	private Integer derivedInteger;

	@Override
	@XmlTransient
	public String getBizModule() {
		return MappedExtensionSingleStrategy.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return MappedExtensionSingleStrategy.DOCUMENT_NAME;
	}

	public static MappedExtensionSingleStrategyExtension newInstance() {
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
			return org.skyve.util.Binder.formatMessage(org.skyve.CORE.getUser().getCustomer(),
														"{text}",
														this);
		}
		catch (@SuppressWarnings("unused") Exception e) {
			return "Unknown";
		}
	}

	@Override
	public boolean equals(Object o) {
		return ((o instanceof MappedExtensionSingleStrategy) && 
					this.getBizId().equals(((MappedExtensionSingleStrategy) o).getBizId()));
	}

	/**
	 * {@link #aggregatedAssociation} accessor.
	 * @return	The value.
	 **/
	public MappedExtensionSingleStrategyExtension getAggregatedAssociation() {
		return aggregatedAssociation;
	}

	/**
	 * {@link #aggregatedAssociation} mutator.
	 * @param aggregatedAssociation	The new value.
	 **/
	@XmlElement
	public void setAggregatedAssociation(MappedExtensionSingleStrategyExtension aggregatedAssociation) {
		if (this.aggregatedAssociation != aggregatedAssociation) {
			preset(aggregatedAssociationPropertyName, aggregatedAssociation);
			MappedExtensionSingleStrategyExtension oldAggregatedAssociation = this.aggregatedAssociation;
			this.aggregatedAssociation = aggregatedAssociation;
			if ((aggregatedAssociation != null) && (aggregatedAssociation.getInverseAggregatedAssociationElementById(getBizId()) == null)) {
				aggregatedAssociation.getInverseAggregatedAssociation().add((MappedExtensionSingleStrategyExtension) this);
			}
			if (oldAggregatedAssociation != null) {
				oldAggregatedAssociation.getInverseAggregatedAssociation().remove(this);
			}
		}
	}

	public void nullAggregatedAssociation() {
		this.aggregatedAssociation = null;
	}

	/**
	 * {@link #aggregatedCollection} accessor.
	 * @return	The value.
	 **/
	@XmlElement
	public List<MappedExtensionSingleStrategyExtension> getAggregatedCollection() {
		return aggregatedCollection;
	}

	/**
	 * {@link #aggregatedCollection} accessor.
	 * @param bizId	The bizId of the element in the list.
	 * @return	The value of the element in the list.
	 **/
	public MappedExtensionSingleStrategyExtension getAggregatedCollectionElementById(String bizId) {
		return getElementById(aggregatedCollection, bizId);
	}

	/**
	 * {@link #aggregatedCollection} mutator.
	 * @param bizId	The bizId of the element in the list.
	 * @param element	The new value of the element in the list.
	 **/
	public void setAggregatedCollectionElementById(String bizId, MappedExtensionSingleStrategyExtension element) {
		setElementById(aggregatedCollection, element);
	}

	/**
	 * {@link #aggregatedCollection} add.
	 * @param element	The element to add.
	 **/
	public boolean addAggregatedCollectionElement(MappedExtensionSingleStrategyExtension element) {
		return aggregatedCollection.add(element);
	}

	/**
	 * {@link #aggregatedCollection} add.
	 * @param index	The index in the list to add the element to.
	 * @param element	The element to add.
	 **/
	public void addAggregatedCollectionElement(int index, MappedExtensionSingleStrategyExtension element) {
		aggregatedCollection.add(index, element);
	}

	/**
	 * {@link #aggregatedCollection} remove.
	 * @param element	The element to remove.
	 **/
	public boolean removeAggregatedCollectionElement(MappedExtensionSingleStrategyExtension element) {
		return aggregatedCollection.remove(element);
	}

	/**
	 * {@link #aggregatedCollection} remove.
	 * @param index	The index in the list to remove the element from.
	 **/
	public MappedExtensionSingleStrategyExtension removeAggregatedCollectionElement(int index) {
		return aggregatedCollection.remove(index);
	}

	/**
	 * {@link #composedCollection} accessor.
	 * @return	The value.
	 **/
	@XmlElement
	public List<MappedExtensionSingleStrategyExtension> getComposedCollection() {
		return composedCollection;
	}

	/**
	 * {@link #composedCollection} accessor.
	 * @param bizId	The bizId of the element in the list.
	 * @return	The value of the element in the list.
	 **/
	public MappedExtensionSingleStrategyExtension getComposedCollectionElementById(String bizId) {
		return getElementById(composedCollection, bizId);
	}

	/**
	 * {@link #composedCollection} mutator.
	 * @param bizId	The bizId of the element in the list.
	 * @param element	The new value of the element in the list.
	 **/
	public void setComposedCollectionElementById(String bizId, MappedExtensionSingleStrategyExtension element) {
		setElementById(composedCollection, element);
	}

	/**
	 * {@link #composedCollection} add.
	 * @param element	The element to add.
	 **/
	public boolean addComposedCollectionElement(MappedExtensionSingleStrategyExtension element) {
		return composedCollection.add(element);
	}

	/**
	 * {@link #composedCollection} add.
	 * @param index	The index in the list to add the element to.
	 * @param element	The element to add.
	 **/
	public void addComposedCollectionElement(int index, MappedExtensionSingleStrategyExtension element) {
		composedCollection.add(index, element);
	}

	/**
	 * {@link #composedCollection} remove.
	 * @param element	The element to remove.
	 **/
	public boolean removeComposedCollectionElement(MappedExtensionSingleStrategyExtension element) {
		return composedCollection.remove(element);
	}

	/**
	 * {@link #composedCollection} remove.
	 * @param index	The index in the list to remove the element from.
	 **/
	public MappedExtensionSingleStrategyExtension removeComposedCollectionElement(int index) {
		return composedCollection.remove(index);
	}

	/**
	 * {@link #inverseAggregatedAssociation} accessor.
	 * @return	The value.
	 **/
	@XmlElement
	public List<MappedExtensionSingleStrategyExtension> getInverseAggregatedAssociation() {
		return inverseAggregatedAssociation;
	}

	/**
	 * {@link #inverseAggregatedAssociation} accessor.
	 * @param bizId	The bizId of the element in the list.
	 * @return	The value of the element in the list.
	 **/
	public MappedExtensionSingleStrategyExtension getInverseAggregatedAssociationElementById(String bizId) {
		return getElementById(inverseAggregatedAssociation, bizId);
	}

	/**
	 * {@link #inverseAggregatedAssociation} mutator.
	 * @param bizId	The bizId of the element in the list.
	 * @param element	The new value of the element in the list.
	 **/
	public void setInverseAggregatedAssociationElementById(String bizId, MappedExtensionSingleStrategyExtension element) {
		setElementById(inverseAggregatedAssociation, element);
	}

	/**
	 * {@link #inverseAggregatedAssociation} add.
	 * @param element	The element to add.
	 **/
	public boolean addInverseAggregatedAssociationElement(MappedExtensionSingleStrategyExtension element) {
		boolean result = false;
		if (getElementById(inverseAggregatedAssociation, element.getBizId()) == null) {
			result = inverseAggregatedAssociation.add(element);
		}
		element.setAggregatedAssociation((MappedExtensionSingleStrategyExtension) this);
		return result;
	}

	/**
	 * {@link #inverseAggregatedAssociation} add.
	 * @param index	The index in the list to add the element to.
	 * @param element	The element to add.
	 **/
	public void addInverseAggregatedAssociationElement(int index, MappedExtensionSingleStrategyExtension element) {
		inverseAggregatedAssociation.add(index, element);
		element.setAggregatedAssociation((MappedExtensionSingleStrategyExtension) this);
	}

	/**
	 * {@link #inverseAggregatedAssociation} remove.
	 * @param element	The element to remove.
	 **/
	public boolean removeInverseAggregatedAssociationElement(MappedExtensionSingleStrategyExtension element) {
		boolean result = inverseAggregatedAssociation.remove(element);
		if (result) {
			element.nullAggregatedAssociation();
		}
		return result;
	}

	/**
	 * {@link #inverseAggregatedAssociation} remove.
	 * @param index	The index in the list to remove the element from.
	 **/
	public MappedExtensionSingleStrategyExtension removeInverseAggregatedAssociationElement(int index) {
		MappedExtensionSingleStrategyExtension result = inverseAggregatedAssociation.remove(index);
		result.nullAggregatedAssociation();
		return result;
	}

	/**
	 * {@link #derivedInteger} accessor.
	 * @return	The value.
	 **/
	public Integer getDerivedInteger() {
		return derivedInteger;
	}

	/**
	 * {@link #derivedInteger} mutator.
	 * @param derivedInteger	The new value.
	 **/
	@XmlElement
	public void setDerivedInteger(Integer derivedInteger) {
		this.derivedInteger = derivedInteger;
	}
}
