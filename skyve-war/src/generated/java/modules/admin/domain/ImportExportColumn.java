package modules.admin.domain;

import jakarta.annotation.Generated;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlEnum;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import modules.admin.ImportExport.ImportExportExtension;
import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.ChildBean;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.types.Enumeration;
import org.skyve.impl.domain.AbstractPersistentBean;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.util.Util;

/**
 * Import Export Column
 * 
 * @depend - - - LoadAction
 * @stereotype "persistent child"
 */
@XmlType
@XmlRootElement
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
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

	/** @hidden */
	public static final String loadActionPropertyName = "loadAction";

	/**
	 * Action
	 **/
	@XmlEnum
	@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
	public static enum LoadAction implements Enumeration {
		setValue("set", "admin.importExportColumn.loadAction.set.description"),
		lookupEquals("equals", "admin.importExportColumn.loadAction.equals.description"),
		lookupLike("like", "admin.importExportColumn.loadAction.like.description"),
		lookupContains("contains", "admin.importExportColumn.loadAction.contains.description"),
		confirmValue("confirm", "admin.importExportColumn.loadAction.confirm.description");

		private String code;
		private String description;

		/** @hidden */
		private DomainValue domainValue;

		/** @hidden */
		private static List<DomainValue> domainValues = Stream.of(values()).map(LoadAction::toDomainValue).collect(Collectors.toUnmodifiableList());

		private LoadAction(String code, String description) {
			this.code = code;
			this.description = description;
			this.domainValue = new DomainValue(code, description);
		}

		@Override
		public String toCode() {
			return code;
		}

		@Override
		public String toLocalisedDescription() {
			return Util.i18n(description);
		}

		@Override
		public DomainValue toDomainValue() {
			return domainValue;
		}

		public static LoadAction fromCode(String code) {
			LoadAction result = null;

			for (LoadAction value : values()) {
				if (value.code.equals(code)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static LoadAction fromLocalisedDescription(String description) {
			LoadAction result = null;

			for (LoadAction value : values()) {
				if (value.toLocalisedDescription().equals(description)) {
					result = value;
					break;
				}
			}

			return result;
		}

		public static List<DomainValue> toDomainValues() {
			return domainValues;
		}
	}

	/**
	 * Column Title
	 **/
	private String columnName;

	/**
	 * Binding Name
	 **/
	private String bindingName;

	/**
	 * Expression
	 * <br/>
	 * <strong>An expression using bindings relative to the document.</strong>
For exports, you can use compound expressions using bindings and literals, for example: <i>{name} ({mobile})</i>
	 **/
	private String bindingExpression;

	/**
	 * Action
	 **/
	private LoadAction loadAction;

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
			return org.skyve.util.Binder.formatMessage("Column {columnName}", this);
		}
		catch (@SuppressWarnings("unused") Exception e) {
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
	 * {@link #loadAction} accessor.
	 * @return	The value.
	 **/
	public LoadAction getLoadAction() {
		return loadAction;
	}

	/**
	 * {@link #loadAction} mutator.
	 * @param loadAction	The new value.
	 **/
	@XmlElement
	public void setLoadAction(LoadAction loadAction) {
		preset(loadActionPropertyName, loadAction);
		this.loadAction = loadAction;
	}

	/**
	 * Whether to show binding expressions
	 *
	 * @return The condition
	 */
	@XmlTransient
	public boolean isShowExpression() {
		return (bindingName!=null
				&& modules.admin.ImportExportColumn.ImportExportColumnBizlet.EXPRESSION.equals(bindingName));
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
