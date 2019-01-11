package modules.admin.domain;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;
import modules.admin.ImportExport.ImportExportExtension;
import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.ChildBean;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.domain.AbstractPersistentBean;

/**
 * Import Export Column
 * 
 * @stereotype "persistent child"
 */
@XmlType
@XmlRootElement
public class ImportExportColumn extends AbstractPersistentBean implements ChildBean<ImportExportExtension> {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "admin";
	/** @hidden */
	public static final String DOCUMENT_NAME = "ImportExportColumn";

	/** @hidden */
	public static final String columnNamePropertyName = "columnName";
	/** @hidden */
	public static final String bindingNamePropertyName = "bindingName";
	/** @hidden */
	public static final String bindingExpressionPropertyName = "bindingExpression";

	/**
	 * Column Title
	 **/
	private String columnName;
	/**
	 * Binding
	 **/
	private String bindingName;
	/**
	 * Expression
	 * <br/>
	 * <strong>An expression using bindings relative to the document.</strong>
			For exports, you can use compound expressions using bindings and literals, for example:
			<i>{name} ({mobile})</i>
	 **/
	private String bindingExpression;
	private ImportExportExtension parent;

	private Integer bizOrdinal;


	@Override
	@XmlTransient
	public String getBizModule() {
		return ImportExportColumn.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return ImportExportColumn.DOCUMENT_NAME;
	}

	public static ImportExportColumn newInstance() {
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
														"Column",
														this);
		}
		catch (Exception e) {
			return "Unknown";
		}
	}

	@Override
	public boolean equals(Object o) {
		return ((o instanceof ImportExportColumn) && 
					this.getBizId().equals(((ImportExportColumn) o).getBizId()));
	}

	/**
	 * {@link #columnName} accessor.
	 * @return	The value.
	 **/
	public String getColumnName() {
		return columnName;
	}

	/**
	 * {@link #columnName} mutator.
	 * @param columnName	The new value.
	 **/
	@XmlElement
	public void setColumnName(String columnName) {
		preset(columnNamePropertyName, columnName);
		this.columnName = columnName;
	}

	/**
	 * {@link #bindingName} accessor.
	 * @return	The value.
	 **/
	public String getBindingName() {
		return bindingName;
	}

	/**
	 * {@link #bindingName} mutator.
	 * @param bindingName	The new value.
	 **/
	@XmlElement
	public void setBindingName(String bindingName) {
		preset(bindingNamePropertyName, bindingName);
		this.bindingName = bindingName;
	}

	/**
	 * {@link #bindingExpression} accessor.
	 * @return	The value.
	 **/
	public String getBindingExpression() {
		return bindingExpression;
	}

	/**
	 * {@link #bindingExpression} mutator.
	 * @param bindingExpression	The new value.
	 **/
	@XmlElement
	public void setBindingExpression(String bindingExpression) {
		preset(bindingExpressionPropertyName, bindingExpression);
		this.bindingExpression = bindingExpression;
	}

	/**
	 * Whether to show advanced binding strings
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isShowExpression() {
		return (bindingName!=null  &&
				modules.admin.ImportExportColumn.ImportExportColumnBizlet.ADVANCED.equals(bindingName));
	}

	/**
	 * {@link #isShowExpression} negation.
	 *
	 * @return The negated condition
	 */
	public boolean isNotShowExpression() {
		return (! isShowExpression());
	}

	@Override
	public ImportExportExtension getParent() {
		return parent;
	}

	@Override
	@XmlElement
	public void setParent(ImportExportExtension parent) {
		preset(ChildBean.PARENT_NAME, parent);
		this.parent =  parent;
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
