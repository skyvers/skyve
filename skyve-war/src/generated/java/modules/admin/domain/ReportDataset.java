package modules.admin.domain;

import jakarta.annotation.Generated;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;
import modules.admin.ReportDataset.ReportDatasetExtension;
import modules.admin.ReportTemplate.ReportTemplateExtension;
import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.ChildBean;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.domain.AbstractPersistentBean;

/**
 * Report Dataset
 * <br/>
 * Report Dataset is used to capture data which is injected into a report template. This 
		can be a BizQL or SQL query, or a string constant.
 * 
 * @depend - - - DatasetType
 * @stereotype "persistent child"
 */
@XmlType
@XmlRootElement
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public abstract class ReportDataset extends AbstractPersistentBean implements ChildBean<ReportTemplateExtension>, org.skyve.domain.app.admin.ReportDataset {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "admin";

	/** @hidden */
	public static final String DOCUMENT_NAME = "ReportDataset";

	/** @hidden */
	public static final String datasetNamePropertyName = "datasetName";

	/** @hidden */
	public static final String datasetTypePropertyName = "datasetType";

	/** @hidden */
	public static final String queryPropertyName = "query";

	/** @hidden */
	public static final String resultsPropertyName = "results";

	/**
	 * Dataset Name
	 * <br/>
	 * The name this dataset will be made available to within the report definition.
	 **/
	private String datasetName;

	/**
	 * Dataset Type
	 * <br/>
	 * The type of this dataset; if it is a BizQL or SQL query, a fixed constant value or a dataset Class.
	 **/
	private DatasetType datasetType = DatasetType.bizQL;

	/**
	 * Query
	 * <br/>
	 * The query to retrieve this dataset for the report.
	 **/
	private String query;

	/**
	 * Query Results
	 * <br/>
	 * Results of testing the query.
	 **/
	private String results;

	private ReportTemplateExtension parent;

	private Integer bizOrdinal;

	@Override
	@XmlTransient
	public String getBizModule() {
		return ReportDataset.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return ReportDataset.DOCUMENT_NAME;
	}

	public static ReportDatasetExtension newInstance() {
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
			return org.skyve.util.Binder.formatMessage("Query - {parent.name} {datasetType}", this);
		}
		catch (@SuppressWarnings("unused") Exception e) {
			return "Unknown";
		}
	}

	@Override
	public boolean equals(Object o) {
		return ((o instanceof ReportDataset) && 
					this.getBizId().equals(((ReportDataset) o).getBizId()));
	}

	/**
	 * {@link #datasetName} accessor.
	 * @return	The value.
	 **/
	public String getDatasetName() {
		return datasetName;
	}

	/**
	 * {@link #datasetName} mutator.
	 * @param datasetName	The new value.
	 **/
	@XmlElement
	public void setDatasetName(String datasetName) {
		preset(datasetNamePropertyName, datasetName);
		this.datasetName = datasetName;
	}

	/**
	 * {@link #datasetType} accessor.
	 * @return	The value.
	 **/
	public DatasetType getDatasetType() {
		return datasetType;
	}

	/**
	 * {@link #datasetType} mutator.
	 * @param datasetType	The new value.
	 **/
	@XmlElement
	public void setDatasetType(DatasetType datasetType) {
		preset(datasetTypePropertyName, datasetType);
		this.datasetType = datasetType;
	}

	/**
	 * {@link #query} accessor.
	 * @return	The value.
	 **/
	public String getQuery() {
		return query;
	}

	/**
	 * {@link #query} mutator.
	 * @param query	The new value.
	 **/
	@XmlElement
	public void setQuery(String query) {
		preset(queryPropertyName, query);
		this.query = query;
	}

	/**
	 * {@link #results} accessor.
	 * @return	The value.
	 **/
	public String getResults() {
		return results;
	}

	/**
	 * {@link #results} mutator.
	 * @param results	The new value.
	 **/
	@XmlElement
	public void setResults(String results) {
		this.results = results;
	}

	/**
	 * True when the dataset type is a class, used to hide the results ouput
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isTypeClass() {
		return (DatasetType.classValue == getDatasetType());
	}

	/**
	 * {@link #isTypeClass} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotTypeClass() {
		return (! isTypeClass());
	}

	/**
	 * True when the dataset type is a constant, used to hide the parameters datagrid
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isTypeConstant() {
		return (DatasetType.constant == getDatasetType());
	}

	/**
	 * {@link #isTypeConstant} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotTypeConstant() {
		return (! isTypeConstant());
	}

	/**
	 * True when the dataset type is a BizQL or SQL, used to show the parameters datagrid
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isTypeQuery() {
		return (DatasetType.bizQL == getDatasetType() || DatasetType.SQL == getDatasetType());
	}

	/**
	 * {@link #isTypeQuery} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotTypeQuery() {
		return (! isTypeQuery());
	}

	/**
	 * True when the dataset type is SQL, used to show help to the user
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isTypeSqlQuery() {
		return (DatasetType.SQL == getDatasetType());
	}

	/**
	 * {@link #isTypeSqlQuery} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotTypeSqlQuery() {
		return (! isTypeSqlQuery());
	}

	@Override
	public ReportTemplateExtension getParent() {
		return parent;
	}

	@Override
	@XmlElement
	public void setParent(ReportTemplateExtension parent) {
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
