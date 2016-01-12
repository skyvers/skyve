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
 * @navhas n aggregatedCollection 0..n MappedExtensionJoinedStrategy
 * @navcomposed n composedCollection 0..n MappedExtensionJoinedStrategy
 * @navhas n aggregatedAssociation 0..1 MappedExtensionJoinedStrategy
 * @stereotype "persistent"
 */
@XmlType
@XmlRootElement
public class MappedExtensionJoinedStrategy extends MappedBase {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "test";
	/** @hidden */
	public static final String DOCUMENT_NAME = "MappedExtensionJoinedStrategy";

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

	private MappedExtensionJoinedStrategy aggregatedAssociation = null;
	private List<MappedExtensionJoinedStrategy> aggregatedCollection = new ArrayList<>();
	private List<MappedExtensionJoinedStrategy> composedCollection = new ArrayList<>();
	private List<MappedExtensionJoinedStrategy> inverseAggregatedAssociation = new ArrayList<>();
	private Integer derivedInteger;

	@Override
	@XmlTransient
	public String getBizModule() {
		return MappedExtensionJoinedStrategy.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return MappedExtensionJoinedStrategy.DOCUMENT_NAME;
	}

	public static MappedExtensionJoinedStrategy newInstance() throws Exception {
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
		return ((o instanceof MappedExtensionJoinedStrategy) && 
					this.getBizId().equals(((MappedExtensionJoinedStrategy) o).getBizId()));
	}

	/**
	 * {@link #aggregatedAssociation} accessor.
	 **/
	public MappedExtensionJoinedStrategy getAggregatedAssociation() {
		return aggregatedAssociation;
	}

	/**
	 * {@link #aggregatedAssociation} mutator.
	 * 
	 * @param aggregatedAssociation	The new value to set.
	 **/
	@XmlElement
	public void setAggregatedAssociation(MappedExtensionJoinedStrategy aggregatedAssociation) {
		preset(aggregatedAssociationPropertyName, aggregatedAssociation);
		this.aggregatedAssociation = aggregatedAssociation;
	}

	/**
	 * {@link #aggregatedCollection} accessor.
	 **/
	@XmlElement
	public List<MappedExtensionJoinedStrategy> getAggregatedCollection() {
		return aggregatedCollection;
	}

	/**
	 * {@link #aggregatedCollection} accessor.
	 * 
	 * @param bizId	The bizId of the element in the list.
	 **/
	public MappedExtensionJoinedStrategy getAggregatedCollectionElementById(String bizId) {
		return getElementById(aggregatedCollection, bizId);
	}

	/**
	 * {@link #aggregatedCollection} mutator.
	 * 
	 * @param bizId	The bizId of the element in the list.
	 * @param aggregatedCollection	The new value to set.
	 **/
	public void setAggregatedCollectionElementById(@SuppressWarnings("unused") String bizId, MappedExtensionJoinedStrategy element) {
		 setElementById(aggregatedCollection, element);
	}

	/**
	 * {@link #composedCollection} accessor.
	 **/
	@XmlElement
	public List<MappedExtensionJoinedStrategy> getComposedCollection() {
		return composedCollection;
	}

	/**
	 * {@link #composedCollection} accessor.
	 * 
	 * @param bizId	The bizId of the element in the list.
	 **/
	public MappedExtensionJoinedStrategy getComposedCollectionElementById(String bizId) {
		return getElementById(composedCollection, bizId);
	}

	/**
	 * {@link #composedCollection} mutator.
	 * 
	 * @param bizId	The bizId of the element in the list.
	 * @param composedCollection	The new value to set.
	 **/
	public void setComposedCollectionElementById(@SuppressWarnings("unused") String bizId, MappedExtensionJoinedStrategy element) {
		 setElementById(composedCollection, element);
	}

	/**
	 * {@link #inverseAggregatedAssociation} accessor.
	 **/
	@XmlElement
	public List<MappedExtensionJoinedStrategy> getInverseAggregatedAssociation() {
		return inverseAggregatedAssociation;
	}

	/**
	 * {@link #inverseAggregatedAssociation} accessor.
	 * 
	 * @param bizId	The bizId of the element in the list.
	 **/
	public MappedExtensionJoinedStrategy getInverseAggregatedAssociationElementById(String bizId) {
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
