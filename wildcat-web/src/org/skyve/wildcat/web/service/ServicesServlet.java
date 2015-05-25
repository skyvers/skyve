package org.skyve.wildcat.web.service;

import javax.servlet.ServletConfig;
/*
import org.apache.cxf.Bus;
import org.apache.cxf.BusFactory;
import org.apache.cxf.frontend.ServerFactoryBean;
import org.apache.cxf.transport.servlet.CXFNonSpringServlet;
*/
import org.skyve.metadata.customer.Customer;
//import org.skyve.metadata.customer.Service;
import org.skyve.wildcat.metadata.repository.AbstractRepository;

/**
 * Java EE Restful or SOAP services are used now instead of this...
 */
@Deprecated
public class ServicesServlet /* extends CXFNonSpringServlet */ {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = 1L;
/*
	@Override
	public void loadBus(ServletConfig servletConfig) {
		super.loadBus(servletConfig);

		Bus bus = getBus();
		BusFactory.setDefaultBus(bus);

		// use simple front end API to publish
		ServerFactoryBean factory = new ServerFactoryBean();
		factory.setBus(bus);

		AbstractRepository repository = AbstractRepository.get();
		ClassLoader loader = Thread.currentThread().getContextClassLoader();
		Thread.currentThread().setContextClassLoader(loader);
		
		try {
			for (String customerName : repository.getAllCustomerNames()) {
				Customer customer = repository.getCustomer(customerName);
				for (Service service : customer.getServices()) {
//					AegisContext aegisContext = new AegisContext();
//					Set<String> rootClassNames = new TreeSet<String>();
//					rootClassNames.add("customers.bizhub.modules.admin.domain.ContactExt");
//					aegisContext.setRootClassNames(rootClassNames);
//					aegisContext.setReadXsiTypes(true);
//					aegisContext.getBeanImplementationMap().put(loader.loadClass("modules.admin.domain.Contact"),
//																	"customers.bizhub.modules.admin.domain.ContactExt");
//					AegisDatabinding aegisDatabinding = new AegisDatabinding(aegisContext);
//					factory.getServiceFactory().setDataBinding(aegisDatabinding);

					factory.setServiceClass(loader.loadClass(service.getClassName()));
					factory.setAddress(new StringBuilder(32).append('/').append(customerName).append('/').append(service.getName()).toString());
					factory.create();
					
//					Endpoint.publish(new StringBuilder(32).append('/').append(moduleName).append('/').append(service.getName()).toString(),
//										loader.loadClass(service.getClassName()).newInstance());
				}
			}
		}
		catch (Exception e) {
			throw new IllegalStateException("Could not publish services", e);
		}
	}

/* But what principal?  Should be able to have a web service that is public so...
 * I guess the web service should look after the persistence stuff and setting the user etc
	@Override
	protected void invoke(HttpServletRequest request, HttpServletResponse response)
	throws ServletException {
		Persistence persistence = Persistence.get();
		try {
			persistence.begin();
	    	Principal userPrincipal = request.getUserPrincipal();
	    	User user = WebUtil.processUserPrincipalForRequest(request, (userPrincipal == null) ? null : userPrincipal.getName());
	    	persistence.setUser(user);

	    	super.invoke(request, response);
		}
		catch (Exception e) {
			persistence.rollback();
			if (e instanceof ServletException) {
				throw (ServletException) e;
			}
			throw new ServletException("Service invocation encountered an error", e);
		}
		finally {
			persistence.commit(true);
		}
	}
*/
}
