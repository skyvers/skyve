package org.skyve.impl.tools.jasperreports;

import java.util.List;
import java.util.Map;

import net.sf.jasperreports.engine.JRDataset;
import net.sf.jasperreports.engine.JRException;
import net.sf.jasperreports.engine.JRField;
import net.sf.jasperreports.engine.design.JRDesignField;

import org.skyve.CORE;
import org.skyve.impl.bind.BindUtil;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.DocumentQueryDefinition;
import org.skyve.metadata.module.query.QueryColumn;

import com.jaspersoft.ireport.designer.FieldsProvider;
import com.jaspersoft.ireport.designer.FieldsProviderEditor;
import com.jaspersoft.ireport.designer.IReportConnection;
import com.jaspersoft.ireport.designer.data.ReportQueryDialog;

public class SkyveQueryFieldsProvider implements FieldsProvider {
    @Override
	public String designQuery(IReportConnection connection, String query, ReportQueryDialog dialog) 
	throws JRException, UnsupportedOperationException {
	    return null; // not used
    }

    @Override
	public FieldsProviderEditor getEditorComponent(ReportQueryDialog dialog) {
	    return null; // not used
    }

	@Override
    public JRField[] getFields(IReportConnection connection, JRDataset dataset, Map parameters) 
	throws JRException, UnsupportedOperationException {
    	try {
    		String moduleDotQuery = dataset.getQuery().getText();
    		DocumentQueryDefinition query = SkyveQueryExecuter.getQuery(moduleDotQuery);
    		
    		Customer customer = CORE.getUser().getCustomer();
    		Module owningModule = query.getOwningModule();
    		Document document = owningModule.getDocument(customer, query.getDocumentName());
    		
    		List<QueryColumn> columns = query.getColumns();
    		JRField[] result = new JRField[columns.size()];
    		for (int i = 0, l = columns.size(); i < l; i++) {
    			QueryColumn column = columns.get(i);
    			JRDesignField field = new JRDesignField();
    			String binding = column.getBinding();
    			field.setName(binding.replace('.', '_'));
    			
    			Attribute attribute = BindUtil.getMetaDataForBinding(customer, owningModule, document, binding).getAttribute();
    			Class<?> propertyType = attribute.getAttributeType().getImplementingType();
    			field.setValueClass(propertyType);
    			field.setValueClassName(propertyType.getName());
    			result[i] = field;
    		}
    
    		return result;
		}
		catch (MetaDataException e) {
			throw new JRException("Could not get fields from BizHubQueryFieldsProvider", e);
		}
    }

	@Override
    public boolean hasEditorComponent() {
	    return false;
    }

	@Override
    public boolean hasQueryDesigner() {
	    return false;
    }

	@Override
    public boolean supportsAutomaticQueryExecution() {
	    return false;
    }

	@Override
    public boolean supportsGetFieldsOperation() {
		return true;
    }
}

