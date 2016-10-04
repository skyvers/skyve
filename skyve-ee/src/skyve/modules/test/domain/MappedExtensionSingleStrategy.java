package modules.test.domain;

import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;
import modules.test.domain.MappedBase;
import org.skyve.CORE;

/**
 * Extension document.
 * 
 * @navhas n aggregatedCollection 0..n MappedExtensionSingleStrategy
 * @navcomposed n composedCollection 0..n MappedExtensionSingleStrategy
 * @navhas n aggregatedAssociation 0..1 MappedExtensionSingleStrategy
 * @stereotype "persistent"
 */
@XmlType
@XmlRootElement
public class MappedExtensionSingleStrategy extends MappedBase {
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

	private MappedExtensionSingleStrategy aggregatedAssociation = null;
	private List<MappedExtensionSingleStrategy> aggregatedCollection = new ArrayList<>();
	private List<MappedExtensionSingleStrategy> composedCollection = new ArrayList<>();
	private List<MappedExtensionSingleStrategy> inverseAggregatedAssociation = new ArrayList<>();
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

	public static MappedExtensionSingleStrategy newInstance() throws Exception {
		return CORE.getUser().getCustomer().getModule(MODULE_NAME).getDocument(CORE.getUser().getCustomer(), DOCUMENT_NAME).newInstance(CORE.getUser());
	}

	@Override
	@XmlTransient
	public String getBizKey() {
		try {
			return org.skyve.util.Binder.formatMessage(org.skyve.CORE.getUser().getCustomer(),
														"{text}",
														this);
		}
		catch (Exception e) {
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
	 **/
	public MappedExtensionSingleStrategy getAggregatedAssociation() {
		return aggregatedAssociation;
	}

	/**
	 * {@link #aggregatedAssociation} mutator.
	 * 
	 * @param aggregatedAssociation	The new value to set.
	 **/
	@XmlElement
	public void setAggregatedAssociation(MappedExtensionSingleStrategy aggregatedAssociation) {
		preset(aggregatedAssociationPropertyName, aggregatedAssociation);
		this.aggregatedAssociation = aggregatedAssociation;
	}

	/**
	 * {@link #aggregatedCollection} accessor.
	 **/
	@XmlElement
	public List<MappedExtensionSingleStrategy> getAggregatedCollection() {
		return aggregatedCollection;
	}

	/**
	 * {@link #aggregatedCollection} accessor.
	 * 
	 * @param bizId	The bizId of the element in the list.
	 **/
	public MappedExtensionSingleStrategy getAggregatedCollectionElementById(String bizId) {
		return getElementById(aggregatedCollection, bizId);
	}

	/**
	 * {@link #aggregatedCollection} mutator.
	 * 
	 * @param bizId	The bizId of the element in the list.
	 * @param aggregatedCollection	The new value to set.
	 **/
	public void setAggregatedCollectionElementById(@SuppressWarnings("unused") String bizId, MappedExtensionSingleStrategy element) {
		 setElementById(aggregatedCollection, element);
	}

	/**
	 * {@link #composedCollection} accessor.
	 **/
	@XmlElement
	public List<MappedExtensionSingleStrategy> getComposedCollection() {
		return composedCollection;
	}

	/**
	 * {@link #composedCollection} accessor.
	 * 
	 * @param bizId	The bizId of the element in the list.
	 **/
	public MappedExtensionSingleStrategy getComposedCollectionElementById(String bizId) {
		return getElementById(composedCollection, bizId);
	}

	/**
	 * {@link #composedCollection} mutator.
	 * 
	 * @param bizId	The bizId of the element in the list.
	 * @param composedCollection	The new value to set.
	 **/
	public void setComposedCollectionElementById(@SuppressWarnings("unused") String bizId, MappedExtensionSingleStrategy element) {
		 setElementById(composedCollection, element);
	}

	/**
	 * {@link #inverseAggregatedAssociation} accessor.
	 **/
	@XmlElement
	public List<MappedExtensionSingleStrategy> getInverseAggregatedAssociation() {
		return inverseAggregatedAssociation;
	}

	/**
	 * {@link #inverseAggregatedAssociation} accessor.
	 * 
	 * @param bizId	The bizId of the element in the list.
	 **/
	public MappedExtensionSingleStrategy getInverseAggregatedAssociationElementById(String bizId) {
		return getElementById(inverseAggregatedAssociation, bizId);
	}

	/**
	 * {@link #derivedInteger} accessor.
	 **/
	public Integer getDerivedInteger() {
		return derivedInteger;
	}

	/**
	 * {@link #derivedInteger} mutator.
	 * 
	 * @param derivedInteger	The new value to set.
	 **/
	@XmlElement
	public void setDerivedInteger(Integer derivedInteger) {
		this.derivedInteger = derivedInteger;
	}
}
