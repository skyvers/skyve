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

/**
 * Resolves JasperReports fields from a Skyve document identifier.
 *
 * <p>The report query text is expected to be in {@code module.document}
 * format. The provider inspects the document metadata and returns one
 * {@link JRField} per attribute.
 */
public class SkyveDocumentFieldsProvider implements FieldsProvider {
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
	 * Builds the field list from all attributes of the referenced document.
	 *
	 * @param connection ignored by this implementation
	 * @param dataset the dataset containing {@code module.document} query text
	 * @param parameters ignored by this implementation
	 * @return report fields mapped from document attributes
	 * @throws JRException if metadata cannot be resolved
	 */
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
	
	/**
	 * Resolves a Skyve document from {@code module.document} notation.
	 *
	 * @param moduleDotDocument document selector in {@code module.document} format
	 * @return the resolved document metadata
	 */
	public static Document getDocument(String moduleDotDocument) {
		User user = SkyveDocumentExecuterFactory.getUser();
		
		int dotIndex = moduleDotDocument.indexOf('.');
		Customer customer = user.getCustomer();
		Module module = customer.getModule(moduleDotDocument.substring(0, dotIndex));
		return module.getDocument(customer, moduleDotDocument.substring(dotIndex + 1));
	}
}
