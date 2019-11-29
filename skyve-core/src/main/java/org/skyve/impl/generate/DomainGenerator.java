package org.skyve.impl.generate;

import java.util.Map.Entry;

import org.apache.commons.lang3.StringUtils;
import org.skyve.impl.metadata.repository.AbstractRepository;
import org.skyve.impl.metadata.repository.LocalDesignRepository;
import org.skyve.impl.metadata.repository.router.Router;
import org.skyve.impl.metadata.repository.router.UxUiMetadata;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.Module.DocumentRef;
import org.skyve.metadata.view.View;
import org.skyve.metadata.view.View.ViewType;

public abstract class DomainGenerator {
	protected static final String DECIMAL2 = "Decimal2";
	protected static final String DECIMAL5 = "Decimal5";
	protected static final String DECIMAL10 = "Decimal10";
	protected static final String DATE_ONLY = "DateOnly";
	protected static final String DATE_TIME = "DateTime";
	protected static final String TIME_ONLY = "TimeOnly";
	protected static final String TIMESTAMP = "Timestamp";
	protected static final String GEOMETRY = "Geometry";

	protected String srcPath;
	protected String generatedSrcPath;
	protected String testPath;
	protected String generatedTestPath;
	protected String[] excludedModules;

	protected DialectOptions dialectOptions = DialectOptions.H2_NO_INDEXES;

	protected AbstractRepository repository;
	
	protected DomainGenerator(AbstractRepository repository,
								DialectOptions dialectOptions,
								String srcPath,
								String generatedSrcPath,
								String testPath,
								String generatedTestPath,
								String[] excludedModules) {
		this.repository = repository;
		this.dialectOptions = dialectOptions;
		this.srcPath = srcPath;
		this.generatedSrcPath = generatedSrcPath;
		this.testPath = testPath;
		this.generatedTestPath = generatedTestPath;
		this.excludedModules = excludedModules;
	}
	
	public void validate(String customerName, boolean pre) throws Exception {
		System.out.println("Get customer " + customerName);
		Customer customer = repository.getCustomer(customerName);
		System.out.println("Validate customer " + customerName);
		repository.validateCustomerForGenerateDomain(customer, pre);
		for (Module module : customer.getModules()) {
			System.out.println("Validate module " + module.getName());
			repository.validateModuleForGenerateDomain(customer, module, pre);
			for (Entry<String, DocumentRef> entry : module.getDocumentRefs().entrySet()) {
				String documentName = entry.getKey();
				System.out.println("Get document " + documentName);
				Document document = module.getDocument(customer, documentName);
				System.out.println("Validate document " + documentName);
				repository.validateDocumentForGenerateDomain(customer, document, pre);
				if (repository.getGlobalRouter().getUxuiSelectorClassName() == null) {
					throw new MetaDataException("uxuiSelectorClassName attribute must be defined in the global router.");
				}
				for (Router moduleRouter : repository.getModuleRouters()) {
					if (moduleRouter.getUxuiSelectorClassName() != null) {
						throw new MetaDataException("uxuiSelectorClassName attribute must only be defined in the global router.");
					}
				}
				for (UxUiMetadata uxui : repository.getRouter().getUxUis()) {
					String uxuiName = uxui.getName();
					System.out.println("Get edit view for document " + documentName + " and uxui " + uxuiName);
					View view = repository.getView(uxuiName, customer, document, ViewType.edit.toString());
					System.out.println("Validate edit view for document " + documentName + " and uxui " + uxuiName);
					repository.validateViewForGenerateDomain(customer, document, view, uxuiName, pre);
					view = repository.getView(uxuiName, customer, document, ViewType.create.toString());
					if (view != null) {
						System.out.println("Validate create view for document " + documentName + " and uxui " + uxuiName);
						repository.validateViewForGenerateDomain(customer, document, view, uxuiName, pre);
					}
				}
			}
		}
	}

	public abstract void generate() throws Exception;

	/**
	 * Usage :-
	 * <dl>
	 * <dt>sourcePath</dt>
	 * <dd>path to source files where modules are located</dd>
	 * <dt>generatedPath</dt>
	 * <dd>path to place generated source files
	 * <dt>testPath</dt>
	 * <dd>path to test files</dd>
	 * <dt>generatedTestPath</dt>
	 * <dd>path to place generated tests (can be same as <code>generatedPath</code>)</dd>
	 * <dt>debug</dt>
	 * <dd>optional, true or false to enable debug mode</dd>
	 * <dt>dialect</dt>
	 * <dd>optional, select which database dialect to use, defaults to H2 by default</dd>
	 * <dt>excludedModules</dt>
	 * <dd>optional, comma separated list of modules not to generate unit tests for</dd>
	 * </dl>
	 * 
	 * E.g. src/skyve src/generated src/test src/generatedTest,
	 * src/skyve src/generated src/test src/generatedTest true
	 * src/skyve src/generated src/test src/generatedTest true MYSQL test,whosin
	 * src/skyve src/generated src/test src/generatedTest false MYSQL whosin
	 * 
	 * @param args
	 * @throws Exception
	 */
	public static void main(String[] args) throws Exception {
		if (args.length == 0 || args.length < 4) {
			System.err.println("You must have at least the src path, generated path, test path and generated test path as arguments"
					+ " - usually \"src/main/java/ src/generated/java/ src/test/java/ src/generatedTest/java/\"");
			System.exit(1);
		}

		UtilImpl.COMMAND_TRACE = false;
		UtilImpl.CONTENT_TRACE = false;
		UtilImpl.HTTP_TRACE = false;
		UtilImpl.QUERY_TRACE = false;
		UtilImpl.SECURITY_TRACE = false;
		UtilImpl.BIZLET_TRACE = false;
		UtilImpl.SQL_TRACE = false;
		UtilImpl.XML_TRACE = false;

		String srcPath = args[0];
		String generatedSrcPath = args[1];// "src/generated/";
		String testPath = args[2];// "src/test/";
		String generatedTestPath = args[3];// "src/generatedTest/";

		if (args.length >= 5) { // allow for debug mode if there are 4 arguments
			if ("true".equalsIgnoreCase(args[4]) || "false".equalsIgnoreCase(args[4])) {
				if (Boolean.parseBoolean(args[4])) {
					UtilImpl.COMMAND_TRACE = true;
					UtilImpl.CONTENT_TRACE = true;
					UtilImpl.HTTP_TRACE = true;
					UtilImpl.QUERY_TRACE = true;
					UtilImpl.SECURITY_TRACE = true;
					UtilImpl.BIZLET_TRACE = true;
					UtilImpl.SQL_TRACE = true;
					UtilImpl.XML_TRACE = true;
				}
			} else {
				System.err.println("The fifth argument DEBUG should be true or false");
				System.exit(1);
			}
		}

		DialectOptions dialectOptions = DialectOptions.H2_NO_INDEXES;
		if (args.length >= 6) {
			try {
				dialectOptions = DialectOptions.valueOf(args[5]);
			}
			catch (@SuppressWarnings("unused") IllegalArgumentException e) {
				System.err.println("The sixth argument DIALECT_OPTIONS should be one of the following "
						+ StringUtils.join(DialectOptions.values(), ", "));
				System.exit(1);
			}
		}

		String[] excludedModules = null;
		if (args.length == 7) {
			if ((args[6] != null) && (!args[6].isEmpty())) {
				excludedModules = args[6].toLowerCase().split(",");
			}
		}

		System.out.println("SRC PATH=" + srcPath);
		System.out.println("GENERATED SRC PATH=" + generatedSrcPath);
		System.out.println("TEST PATH=" + testPath);
		System.out.println("GENERATED TEST PATH=" + generatedTestPath);
		System.out.println("DEBUG=" + (args.length >= 5 ? args[4] : "false"));
		System.out.println("DIALECT OPTIONS=" + dialectOptions.toString());
		System.out.println("EXCLUDED MODULES=" + (args.length == 7 ? args[6] : ""));

		AbstractRepository repository = new LocalDesignRepository();
		DomainGenerator foo = UtilImpl.USING_JPA ? 
								new JPADomainGenerator(repository, dialectOptions, srcPath, generatedSrcPath, testPath, generatedTestPath, excludedModules) : 
								new OverridableDomainGenerator(repository, dialectOptions, srcPath, generatedSrcPath, testPath, generatedTestPath, excludedModules);

		// generate for all customers
		for (String customerName : repository.getAllCustomerNames()) {
			foo.validate(customerName, true);
		}
		foo.generate();
		for (String customerName : repository.getAllCustomerNames()) {
			foo.validate(customerName, false);
		}
	}
}
