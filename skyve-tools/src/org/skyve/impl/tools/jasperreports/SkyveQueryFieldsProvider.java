package org.skyve.impl.tools.jasperreports;

import java.util.List;
import java.util.Map;

import org.skyve.CORE;
import org.skyve.impl.bind.BindUtil;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.MetaDataQueryColumn;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;

import com.jaspersoft.ireport.designer.FieldsProvider;
import com.jaspersoft.ireport.designer.FieldsProviderEditor;
import com.jaspersoft.ireport.designer.IReportConnection;
import com.jaspersoft.ireport.designer.data.ReportQueryDialog;

import net.sf.jasperreports.engine.JRDataset;
import net.sf.jasperreports.engine.JRException;
import net.sf.jasperreports.engine.JRField;
import net.sf.jasperreports.engine.design.JRDesignField;

/**
 * Resolves JasperReports fields from a Skyve metadata query definition.
 *
 * <p>The dataset query text is interpreted as {@code module.query}. For each
 * query column this provider emits one {@link JRField} using the sanitised
 * binding name and inferred Skyve attribute type when available.
 */
public class SkyveQueryFieldsProvider implements FieldsProvider {
    /**
     * Returns no query-designer support for this provider.
	 *
	 * @param connection ignored by this implementation
	 * @param query ignored by this implementation
	 * @param dialog ignored by this implementation
	 * @return always {@code null}
	 * @throws JRException if query design fails
     */
    @Override
	public String designQuery(IReportConnection connection, String query, ReportQueryDialog dialog) 
	throws JRException, UnsupportedOperationException {
	    return null; // not used
    }

    /**
     * Returns no editor component because query text is entered directly.
	 *
	 * @param dialog ignored by this implementation
	 * @return always {@code null}
     */
    @Override
	public FieldsProviderEditor getEditorComponent(ReportQueryDialog dialog) {
	    return null; // not used
    }

	/**
	 * Builds report fields from metadata query columns.
	 *
	 * @param connection ignored by this implementation
	 * @param dataset dataset containing {@code module.query} text
	 * @param parameters ignored by this implementation
	 * @return report fields for each query column
	 * @throws JRException if metadata lookup fails
	 */
	@Override
    public JRField[] getFields(IReportConnection connection, JRDataset dataset, Map parameters) 
	throws JRException, UnsupportedOperationException {
    	try {
    		String moduleDotQuery = dataset.getQuery().getText();
    		MetaDataQueryDefinition query = SkyveQueryExecuter.getQuery(moduleDotQuery);
    		
    		Customer customer = CORE.getUser().getCustomer();
    		Module owningModule = query.getOwningModule();
    		Document document = owningModule.getDocument(customer, query.getDocumentName());
    		
    		List<MetaDataQueryColumn> columns = query.getColumns();
    		JRField[] result = new JRField[columns.size()];
    		for (int i = 0, l = columns.size(); i < l; i++) {
    			MetaDataQueryColumn column = columns.get(i);
    			JRDesignField field = new JRDesignField();
    			String binding = column.getBinding();
    			field.setName(BindUtil.sanitiseBinding(binding));
    			
    			Attribute attribute = BindUtil.getMetaDataForBinding(customer, owningModule, document, binding).getAttribute();
    			if (attribute != null) {
	    			Class<?> propertyType = attribute.getImplementingType();
	    			field.setValueClass(propertyType);
	    			field.setValueClassName(propertyType.getName());
    			}
    			result[i] = field;
    		}
    
    		return result;
		}
		catch (MetaDataException e) {
			throw new JRException("Could not get fields from BizHubQueryFieldsProvider", e);
		}
    }

	/**
	 * Indicates that no custom editor component is available.
	 *
	 * @return always {@code false}
	 */
	@Override
    public boolean hasEditorComponent() {
	    return false;
    }

	/**
	 * Indicates that no query-designer UI is available.
	 *
	 * @return always {@code false}
	 */
	@Override
    public boolean hasQueryDesigner() {
	    return false;
    }

	/**
	 * Indicates that fields must be loaded explicitly, not by auto execution.
	 *
	 * @return always {@code false}
	 */
	@Override
    public boolean supportsAutomaticQueryExecution() {
	    return false;
    }

	/**
	 * Indicates support for field introspection operations.
	 *
	 * @return always {@code true}
	 */
	@Override
    public boolean supportsGetFieldsOperation() {
		return true;
    }
}

