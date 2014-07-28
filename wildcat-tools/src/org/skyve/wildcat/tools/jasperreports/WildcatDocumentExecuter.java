package org.skyve.wildcat.tools.jasperreports;

import net.sf.jasperreports.engine.JRDataSource;
import net.sf.jasperreports.engine.JRException;
import net.sf.jasperreports.engine.query.JRQueryExecuter;

import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.wildcat.jasperreports.WildcatDataSource;
import org.skyve.wildcat.util.UtilImpl;

public class WildcatDocumentExecuter implements JRQueryExecuter {
	private String moduleDotDocument;
	
	public WildcatDocumentExecuter(String moduleDotDocument) {
		this.moduleDotDocument = moduleDotDocument;
	}
	
	@Override
	public boolean cancelQuery() 
	throws JRException {
// TODO - allow query to be cancelled
		return false;
	}

	@Override
	public void close() {
		// nothing to do here
	}

	@Override
	public JRDataSource createDatasource() 
	throws JRException {
		try {
			User user = WildcatDocumentExecuterFactory.getUser();

			int dotIndex = moduleDotDocument.indexOf('.');
			Customer customer = user.getCustomer();
			Module module = customer.getModule(moduleDotDocument.substring(0, dotIndex));
			Document document = module.getDocument(customer, moduleDotDocument.substring(dotIndex + 1));
			
			return new WildcatDataSource(user, UtilImpl.constructRandomInstance(user, module, document, 5));
        }
        catch (Exception e) {
        	throw new JRException("Could not create a BizHubQueryDataSource for " + moduleDotDocument, e);
        }
	}
}
