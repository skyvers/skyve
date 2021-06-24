package org.skyve.impl.tools.jasperreports;

import org.skyve.domain.Bean;
import org.skyve.impl.report.jasperreports.SkyveDataSource;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.util.test.TestUtil;

import net.sf.jasperreports.engine.JRDataSource;
import net.sf.jasperreports.engine.JRException;
import net.sf.jasperreports.engine.query.JRQueryExecuter;

public class SkyveDocumentExecuter implements JRQueryExecuter {
	private String moduleDotDocument;
	
	public SkyveDocumentExecuter(String moduleDotDocument) {
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
			User user = SkyveDocumentExecuterFactory.getUser();

			int dotIndex = moduleDotDocument.indexOf('.');
			Customer customer = user.getCustomer();
			Module module = customer.getModule(moduleDotDocument.substring(0, dotIndex));
			Document document = module.getDocument(customer, moduleDotDocument.substring(dotIndex + 1));
			
			Bean bean = TestUtil.constructRandomInstance(user, module, document, 5);
			return new SkyveDataSource(user, bean);
        }
        catch (Exception e) {
        	throw new JRException("Could not create a BizHubQueryDataSource for " + moduleDotDocument, e);
        }
	}
}
