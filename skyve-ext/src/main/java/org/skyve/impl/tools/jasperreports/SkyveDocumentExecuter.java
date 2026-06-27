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

/**
 * Jasper query executer that builds a synthetic Skyve bean and exposes it as a data source.
 */
public class SkyveDocumentExecuter implements JRQueryExecuter {
	/**
	 * Module and document identifier in module.document format.
	 */
	private String moduleDotDocument;
	
	/**
	 * Creates an executer for a module.document reference.
	 *
	 * @param moduleDotDocument The module and document identifier in module.document form.
	 */
	public SkyveDocumentExecuter(String moduleDotDocument) {
		this.moduleDotDocument = moduleDotDocument;
	}
	
	/**
	 * Requests cancellation of query execution.
	 *
	 * @return {@code false}, because cancellation is not currently implemented.
	 * @throws JRException If cancellation fails.
	 */
	@Override
	public boolean cancelQuery() 
	throws JRException {
		// Cancellation is not currently supported by this executer.
		return false;
	}

	/**
	 * Releases resources associated with this executer.
	 */
	@Override
	public void close() {
		// nothing to do here
	}

	/**
	 * Creates a Jasper data source by constructing a synthetic bean for the requested document.
	 *
	 * @return A Skyve-backed Jasper data source.
	 * @throws JRException If the module/document cannot be resolved or data source creation fails.
	 */
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
