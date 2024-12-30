package org.skyve.impl.tools.jasperreports;

import java.util.List;
import java.util.Map;

import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;

import com.jaspersoft.ireport.designer.FieldsProvider;
import com.jaspersoft.ireport.designer.FieldsProviderEditor;
import com.jaspersoft.ireport.designer.IReportConnection;
import com.jaspersoft.ireport.designer.data.ReportQueryDialog;

import net.sf.jasperreports.engine.JRDataset;
import net.sf.jasperreports.engine.JRException;
import net.sf.jasperreports.engine.JRField;
import net.sf.jasperreports.engine.design.JRDesignField;

public class SkyveDocumentFieldsProvider implements FieldsProvider {
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
    		String moduleDotDocument = dataset.getQuery().getText();
    		Document document = getDocument(moduleDotDocument);
    		
    		User user = SkyveDocumentExecuterFactory.getUser();
    		Customer customer = user.getCustomer();
    		List<? extends Attribute> attributes = document.getAllAttributes(customer);
    		JRField[] result = new JRField[attributes.size()];
    		for (int i = 0, l = attributes.size(); i < l; i++) {
    			Attribute attribute = attributes.get(i);
    			JRDesignField field = new JRDesignField();
    			field.setName(attribute.getName());
    			Class<?> propertyType = attribute.getImplementingType();
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
	
	public static Document getDocument(String moduleDotDocument) {
		User user = SkyveDocumentExecuterFactory.getUser();
		
		int dotIndex = moduleDotDocument.indexOf('.');
		Customer customer = user.getCustomer();
		Module module = customer.getModule(moduleDotDocument.substring(0, dotIndex));
		return module.getDocument(customer, moduleDotDocument.substring(dotIndex + 1));
	}
}
