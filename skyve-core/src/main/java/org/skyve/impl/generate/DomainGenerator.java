package org.skyve.impl.generate;

import java.nio.file.Path;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

import org.apache.commons.lang3.StringUtils;
import org.skyve.CORE;
import org.skyve.domain.messages.SkyveException;
import org.skyve.impl.metadata.controller.CustomisationsStaticSingleton;
import org.skyve.impl.metadata.repository.LocalDesignRepository;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.metadata.repository.router.Router;
import org.skyve.impl.metadata.repository.router.UxUiMetadata;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.controller.Customisations;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.Module.DocumentRef;
import org.skyve.metadata.repository.ProvidedRepository;
import org.skyve.metadata.view.View;
import org.skyve.metadata.view.View.ViewType;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Abstract base for source generators that produce domain classes from Skyve
 * metadata.
 *
 * <p>Coordinates module/document traversal and delegates language-specific
 * output to subclasses.
 *
 * <p>Threading: not thread-safe; intended for single-threaded generation runs.
 */
@SuppressWarnings("java:S1192") // Repeated literals are deliberate fragments of generated source and SQL keyword output.
public abstract class DomainGenerator {
	protected static final String DECIMAL2 = "Decimal2";
	protected static final String DECIMAL5 = "Decimal5";
	protected static final String DECIMAL10 = "Decimal10";
	protected static final String DATE_ONLY = "DateOnly";
	protected static final String DATE_TIME = "DateTime";
	protected static final String TIME_ONLY = "TimeOnly";
	protected static final String TIMESTAMP = "Timestamp";
	protected static final String GEOMETRY = "Geometry";

	/**
	 * Array of Java reserved words. Used to checks if an attribute name is valid, e.g. <code>return</code>.
	 */
	public static final Set<String> JAVA_RESERVED_WORDS = Set.of(
			"abstract", "assert", "boolean", "break", "byte", "case",
			"catch", "char", "class", "const", "continue",
			"default", "do", "double", "else", "extends",
			"false", "final", "finally", "float", "for",
			"goto", "if", "implements", "import", "instanceof",
			"int", "interface", "long", "native", "new",
			"null", "package", "private", "protected", "public",
			"return", "short", "static", "strictfp", "super",
			"switch", "synchronized", "this", "throw", "throws",
			"transient", "true", "try", "void", "volatile", "while");

	/**
	 * Array of H2 database reserved words. Used to check if an attribute name is valid, e.g. <code>from</code>.
	 */
	public static final Set<String> H2_RESERVED_WORDS = Set.of(
			"all", "and", "any", "array", "as", "asymmetric", "authorization", "between", "both",
			"case", "cast", "check", "constraint", "cross", "current_catalog", "current_date", "current_path",
			"current_role", "current_schema", "current_time", "current_timestamp", "current_user", "day", "default",
			"distinct", "else", "end", "except", "exists", "false", "fetch", "for", "foreign", "from", "full",
			"group", "groups", "having", "hour", "if", "ilike", "in", "inner", "intersect", "interval", "is",
			"join", "key", "leading", "left", "like", "limit", "localtime", "localtimestamp", "minus", "minute",
			"month", "natural", "not", "null", "offset", "on", "or", "order", "over", "partition", "primary",
			"qualify", "range", "regexp", "right", "row", "rownum", "rows", "second", "select", "session_user",
			"set", "some", "symmetric", "system_user", "table", "to", "top", "trailing", "true", "uescape", "union",
			"unique", "unknown", "user", "using", "value", "values", "when", "where", "window", "with", "year");

	/**
	 * Array of MySQL 5.x database reserved words. Used to check if an attribute name is valid, e.g. <code>from</code>.
	 */
	public static final Set<String> MYSQL_5_RESERVED_WORDS = Set.of(
			"accessible", "add", "all", "alter", "analyze", "and", "as", "asc", "asensitive", "before", "between", "bigint",
			"binary", "blob", "both", "by", "call", "cascade", "case", "change", "char", "character", "check", "collate",
			"column", "condition", "constraint", "continue", "convert", "create", "cross", "current_date", "current_time",
			"current_timestamp", "current_user", "cursor", "database", "databases",
			"day_hour", "day_microsecond", "day_minute", "day_second", "dec", "decimal", "declare", "default", "delayed",
			"delete", "desc", "describe", "deterministic", "distinct", "distinctrow", "div", "double", "drop", "dual", "each",
			"else", "elseif", "enclosed", "escaped", "exists", "exit", "explain", "false", "fetch", "float", "float4", "float8",
			"for", "force", "foreign", "from", "fulltext", "get", "grant", "group", "having", "high_priority",
			"hour_microsecond", "hour_minute", "hour_second", "if", "ignore", "in", "index", "infile", "inner", "inout",
			"insensitive", "insert", "int", "int1", "int2", "int3", "int4", "int8", "integer", "interval", "into",
			"io_after_gtids", "io_before_gtids", "is", "iterate", "join", "key", "keys", "kill", "leading", "leave", "left",
			"like", "limit", "linear", "lines", "load", "localtime", "localtimestamp", "lock", "long", "longblob", "longtext",
			"loop", "low_priority", "master_bind", "master_ssl_verify_server_cert", "match", "maxvalue", "mediumblob",
			"mediumint", "mediumtext", "middleint", "minute_microsecond", "minute_second", "mod", "modifies", "natural", "not",
			"no_write_to_binlog", "null", "numeric", "on", "optimize", "optimizer_costs", "option", "optionally", "or", "order",
			"out", "outer", "outfile", "partition", "precision", "primary", "procedure", "purge", "range", "read", "reads",
			"read_write", "real", "references", "regexp", "release", "rename", "repeat", "replace", "require", "resignal",
			"restrict", "return", "revoke", "right", "rlike", "schema", "schemas", "second_microsecond", "select", "sensitive",
			"separator", "set", "show", "signal", "smallint", "spatial", "specific", "sql", "sqlexception", "sqlstate",
			"sqlwarning", "sql_big_result", "sql_calc_found_rows", "sql_small_result", "ssl", "starting", "stored",
			"straight_join", "table", "terminated", "then", "tinyblob", "tinyint", "tinytext", "to", "trailing", "trigger",
			"true", "undo", "union", "unique", "unlock", "unsigned", "update", "usage", "use", "using", "utc_date", "utc_time",
			"utc_timestamp", "values", "varbinary", "varchar", "varcharacter", "varying", "virtual", "when", "where", "while",
			"with", "write", "xor", "year_month", "generated");

	/**
	 * Array of MySQL 8.x database reserved words. Used to check if an attribute name is valid, e.g. <code>from</code>.
	 */
	@SuppressWarnings("java:S2386") // mutable because it adds onto MySQL 5 reserved words
	public static final Set<String> MYSQL_8_RESERVED_WORDS = new TreeSet<>(MYSQL_5_RESERVED_WORDS);

	/**
	 * Array of SqlServer T-SQL reserved words. Used to check if an attribute name is valid, e.g. <code>from</code>.
	 */
	public static final Set<String> SQL_SERVER_RESERVED_WORDS = Set.of(
			"add", "external", "procedure", "all", "fetch", "public", "alter", "file", "raiserror", "and", "fillfactor", "read",
			"any", "for", "readtext", "as", "foreign", "reconfigure", "asc", "freetext", "references", "authorization",
			"freetexttable", "replication", "backup", "from", "restore", "begin", "full", "restrict", "between", "function",
			"return", "break", "goto", "revert", "browse", "grant", "revoke", "bulk", "group", "right", "by", "having",
			"rollback", "cascade", "holdlock", "rowcount", "case", "identity", "rowguidcol", "check", "identity_insert", "rule",
			"checkpoint", "identitycol", "save", "close", "if", "schema", "clustered", "in", "securityaudit", "coalesce",
			"index", "select", "collate", "inner", "semantickeyphrasetable", "column", "insert",
			"semanticsimilaritydetailstable", "commit", "intersect", "semanticsimilaritytable", "compute", "into",
			"session_user", "constraint", "is", "set", "contains", "join", "setuser", "containstable", "key", "shutdown",
			"continue", "kill", "some", "convert", "left", "statistics", "create", "like", "system_user", "cross", "lineno",
			"table", "current", "load", "tablesample", "current_date", "merge", "textsize", "current_time", "national", "then",
			"current_timestamp", "nocheck", "to", "current_user", "nonclustered", "top", "cursor", "not", "tran", "database",
			"null", "transaction", "dbcc", "nullif", "trigger", "deallocate", "of", "truncate", "declare", "off", "try_convert",
			"default", "offsets", "tsequal", "delete", "on", "union", "deny", "open", "unique", "desc", "opendatasource",
			"unpivot", "disk", "openquery", "update", "distinct", "openrowset", "updatetext", "distributed", "openxml", "use",
			"double", "option", "user", "drop", "or", "values", "dump", "order", "varying", "else", "outer", "view", "end",
			"over", "waitfor", "errlvl", "percent", "when", "escape", "pivot", "where", "except", "plan", "while", "exec",
			"precision", "execute", "primary", "within", "exists", "print", "exit", "proc");

	/**
	 * Array of POSTGRESQL database reserved words. Used to check if an attribute name is valid, e.g. <code>from</code>.
	 */
	public static final Set<String> POSTGRESQL_RESERVED_WORDS = Set.of(
			"all", "analyse", "analyze", "and", "any", "array",
			"as", "asc", "authorization",
			"between", "binary",
			"both", "case", "cast",
			"check",
			"column", "constraint",
			"create", "cross",
			"current_date", "current_role",
			"current_time", "current_timestamp",
			"default", "deferrable",
			"desc", "distinct",
			"do", "else", "end",
			"except",
			"false", "for", "foreign",
			"freeze", "from", "full", "grant", "group",
			"having", "ilike", "in", "initially", "inner",
			"intersect", "into", "is", "isnull",
			"join", "leading", "left",
			"like", "limit", "localtime", "localtimestamp",
			"natural",
			"new", "not", "notnull", "null",
			"off", "offset", "old", "on", "only", "or", "order", "outer",
			"overlaps",
			"placing",
			"primary",
			"references",
			"right",
			"select", "session_user", "similar", "some",
			"symmetric",
			"table", "then", "to", "trailing",
			"true", "union",
			"unique", "user", "using",
			"verbose", "when", "where");

	// Add extra MySQL 8 reserved words on MySQL 5 reserved words
	static {
		String[] mysql8ExtraReserved = {
				"array", "cume_dist", "dense_rank", "empty", "except", "first_value", "grouping", "groups",
				"json_table", "lag", "last_value", "lateral", "lead", "member", "nth_value", "ntile",
				"of", "over", "percent_rank", "rank", "recursive", "row_number", "system", "window"
		};
		MYSQL_8_RESERVED_WORDS.addAll(Arrays.asList(mysql8ExtraReserved));
	}
	
	protected boolean write;
	protected boolean debug;
	protected boolean multiTenant;
	protected String srcPath;
	protected String generatedSrcPath;
	protected String testPath;
	protected String generatedTestPath;
	protected String[] excludedModules;

	protected DialectOptions dialectOptions = DialectOptions.H2_NO_INDEXES;

	protected ProvidedRepository repository;
	
	protected Map<Path, CharSequence> generation = new TreeMap<>();
	
	@SuppressWarnings("java:S107") // Long parameter list preserves the existing framework/API contract.
	protected DomainGenerator(boolean write,
								boolean debug,
								boolean multiTenant,
								@Nonnull DialectOptions dialectOptions,
								@Nonnull String srcPath,
								@Nonnull String generatedSrcPath,
								@Nonnull String testPath,
								@Nonnull String generatedTestPath,
								@Nullable String[] excludedModules) {
		this.write = write;
		this.debug = debug;
		this.multiTenant = multiTenant;
		this.repository = ProvidedRepositoryFactory.get(); // tooling and tests can install the repository afterwards
		this.dialectOptions = dialectOptions;
		this.srcPath = srcPath;
		this.generatedSrcPath = generatedSrcPath;
		this.testPath = testPath;
		this.generatedTestPath = generatedTestPath;
		this.excludedModules = excludedModules;
	}
	
	/**
	 * Validates one customer's complete metadata graph before generation.
	 *
	 * <p>Router assembly-component constraints and effective direct targets are checked once
	 * per customer, including customers with no document references. Per-document edit and create
	 * views are then validated against the same effective router instance.
	 *
	 * <p>Side effects: loads metadata through the configured repository and may populate repository
	 * caches. When debug mode is enabled, progress is written to standard output.
	 *
	 * @param customerName repository customer name; must not be {@code null}
	 * @throws MetaDataException if required customer or router metadata is absent or inconsistent
	 */
	@SuppressWarnings({"java:S3776", "java:S106"}) // Complexity OK, no logger coz command line
	public void validateCustomer(@Nonnull String customerName) {
		if (debug) System.out.println("Get customer " + customerName);
		Customer customer = repository.getCustomer(customerName);
		if (customer == null) {
			throw new MetaDataException(customerName + " does not exist.");
		}
		if (debug) System.out.println("Validate customer " + customerName);
		repository.validateCustomerForGenerateDomain(customer);

		Router globalRouter = repository.getGlobalRouter();
		if (globalRouter == null) {
			throw new MetaDataException("Global router must be defined.");
		}
		if (globalRouter.getUxuiSelectorClassName() == null) {
			throw new MetaDataException("uxuiSelectorClassName attribute must be defined in the global router.");
		}
		List<Router> moduleRouters = repository.getModuleRouters();
		for (Router moduleRouter : moduleRouters) {
			if (moduleRouter.getUxuiSelectorClassName() != null) {
				throw new MetaDataException("uxuiSelectorClassName attribute must only be defined in the global router.");
			}
		}
		Router router = repository.getRouter();
		if (router == null) {
			throw new MetaDataException("Router must be defined.");
		}
		router.validateDirectTargets();

		for (Module module : customer.getModules()) {
			if (debug) System.out.println("Validate module " + module.getName());
			repository.validateModuleForGenerateDomain(customer, module);
			for (Entry<String, DocumentRef> entry : module.getDocumentRefs().entrySet()) {
				String documentName = entry.getKey();
				if (debug) System.out.println("Get document " + documentName);
				Document document = module.getDocument(customer, documentName);
				if (debug) System.out.println("Validate document " + documentName);
				repository.validateDocumentForGenerateDomain(customer, document);
				for (UxUiMetadata uxui : router.getUxUis()) {
					String uxuiName = uxui.getName();
					if (uxuiName == null) {
						throw new MetaDataException("The router defines a UX/UI with no name.");
					}
					if (debug) System.out.println("Get edit view for document " + documentName + " and uxui " + uxuiName);
					View view = repository.getView(uxuiName, customer, document, ViewType.edit.toString());
					if (view != null) {
						if (debug) System.out.println("Validate edit view for document " + documentName + " and uxui " + uxuiName);
						repository.validateViewForGenerateDomain(customer, document, view, uxuiName);
					}
					view = repository.getView(uxuiName, customer, document, ViewType.create.toString());
					if (view != null) {
						if (debug) System.out.println("Validate create view for document " + documentName + " and uxui " + uxuiName);
						repository.validateViewForGenerateDomain(customer, document, view, uxuiName);
					}
				}
			}
		}
	}

	@SuppressWarnings("java:S112") // Exception is OK here
	public abstract void generate() throws Exception;

	@SuppressWarnings("java:S107") // Long parameter list preserves the existing framework/API contract.
	public static final @Nonnull DomainGenerator newDomainGenerator(boolean write,
																	boolean debug,
																	boolean multiTenant,
																	@Nonnull DialectOptions dialectOptions,
																	@Nonnull String srcPath,
																	@Nonnull String generatedSrcPath,
																	@Nonnull String testPath,
																	@Nonnull String generatedTestPath,
																	@Nullable String... excludedModules) {
		return new OverridableDomainGenerator(write,
												debug,
												multiTenant,
												dialectOptions,
												srcPath,
												generatedSrcPath,
												testPath,
												generatedTestPath,
												excludedModules);
	}
	
	/**
	 * Validate a customer.
	 */
	@SuppressWarnings("java:S106") // command line utility can use stdout/stderr
	public static void validate(@Nonnull String customerName) {
		long millis = System.currentTimeMillis();
		DomainGenerator jenny = DomainGenerator.newDomainGenerator(false, false, false, DialectOptions.H2_NO_INDEXES, "", "", "", "", "");
		try {
			jenny.validateCustomer(customerName);
		}
		catch (SkyveException e) {
			throw e;
		}
		catch (Exception e) {
			throw new MetaDataException("Validation problem encountered: " + e.getLocalizedMessage(), e);
		}
        finally {
            System.out.println("Customer " + customerName + " validated in " + (System.currentTimeMillis() - millis) + " millis");
        }
	}
	
	/**
	 * Initialise the Skyve Customisations for forward generation.
	 * This mirrors what the server does at startup via SkyveContextListener and is required at
	 * generate-domain time because that normal startup path is not executed.
	 * Call this before {@link #generate} or {@link #validate} when the project registers custom
	 * expression evaluators or formatters.
	 * @param customisationsClassName fully-qualified class name of a {@link Customisations} implementation,
	 *                                 or {@code null} to use the default (no customisations) implementation.
	 */
	@SuppressWarnings("java:S112") // Exception is OK here
	public static void registerCustomisations(@Nullable String customisationsClassName) throws Exception {
		if (customisationsClassName != null) {
			Customisations customisations = (Customisations) Thread.currentThread().getContextClassLoader().loadClass(customisationsClassName).getDeclaredConstructor().newInstance();
			CustomisationsStaticSingleton.set(customisations);
		}
		else {
			CustomisationsStaticSingleton.setDefault();
		}
		Customisations c = CORE.getCustomisations();
		c.registerCustomExpressions();
		c.registerCustomFormatters();
	}

	/**
	 * Generate the domain model.
	 */
	@SuppressWarnings({"java:S107", "java:S106"}) // Long parameter list preserves the existing framework/API contract.
	public static void generate(boolean validate,
									boolean debug,
									boolean multiTenant,
									@Nonnull DialectOptions dialectOptions,
									@Nonnull String srcPath,
									@Nonnull String generatedSrcPath,
									@Nonnull String testPath,
									@Nonnull String generatedTestPath,
									@Nullable String... excludedModules)
	throws Exception {
		System.out.println("SRC PATH=" + srcPath);
		System.out.println("GENERATED SRC PATH=" + generatedSrcPath);
		System.out.println("TEST PATH=" + testPath);
		System.out.println("GENERATED TEST PATH=" + generatedTestPath);
		System.out.println("DIALECT OPTIONS=" + dialectOptions.toString());
		System.out.println("MULTI-TENANT=" + multiTenant);
		System.out.println("DEBUG=" + debug);
		System.out.println("VALIDATE=" + validate);
		System.out.println("EXCLUDED MODULES=" + (((excludedModules == null) || (excludedModules.length == 0)) ? "" : StringUtils.join(excludedModules, ", ")));

		// Set access control false so that view loading doesn't resolve all the ACLs
		// as this requires instantiation of model classes to get the driving document.
		// These classes may have Skyve dependencies in their constructor such as Persistence,
		// and this will fail at generate time.
		UtilImpl.ACCESS_CONTROL = false;
		
		UtilImpl.COMMAND_TRACE = false;
		UtilImpl.CONTENT_TRACE = false;
		UtilImpl.HTTP_TRACE = false;
		UtilImpl.QUERY_TRACE = false;
		UtilImpl.SECURITY_TRACE = false;
		UtilImpl.BIZLET_TRACE = false;
		UtilImpl.SQL_TRACE = false;
		UtilImpl.XML_TRACE = false;
		
		if (debug) {
			UtilImpl.COMMAND_TRACE = true;
			UtilImpl.CONTENT_TRACE = true;
			UtilImpl.HTTP_TRACE = true;
			UtilImpl.QUERY_TRACE = true;
			UtilImpl.SECURITY_TRACE = true;
			UtilImpl.BIZLET_TRACE = true;
			UtilImpl.SQL_TRACE = true;
			UtilImpl.XML_TRACE = true;
		}
		
		ProvidedRepository repository = new LocalDesignRepository();
		ProvidedRepositoryFactory.set(repository);
		DomainGenerator jenny = newDomainGenerator(true, debug, multiTenant, dialectOptions, srcPath, generatedSrcPath, testPath, generatedTestPath, excludedModules);

		if (validate) {
			// validate all customers before generation
			for (String customerName : repository.getAllCustomerNames()) {
				jenny.validateCustomer(customerName);
			}
		}
		else {
			System.err.println("Metadata validation is disabled for bootstrap domain generation. Compile the generated classes, then regenerate with validation enabled.");
		}
		// generate for all customers
		jenny.generate();
	}
	
	/**
	 * Usage :-
	 * <dl>
	 * <dt>validate</dt>
	 * <dd>whether to validate metadata before generation</dd>
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
	 * E.g. true src/skyve src/generated src/test src/generatedTest,
	 * true src/skyve src/generated src/test src/generatedTest MYSQL
	 * true src/skyve src/generated src/test src/generatedTest MYSQL false true test,whosin
	 *
	 * @param args
	 * @throws Exception
	 */
	@SuppressWarnings({"java:S3776", "java:S106"}) // Complexity OK, command line utility can use stdout/stderr
	public static void main(@Nonnull String[] args) throws Exception {
		if (args.length < 5) {
			System.err.println("You must have at least the validate flag, src path, generated path, test path and generated test path as arguments"
					+ " - usually \"true src/main/java/ src/generated/java/ src/test/java/ src/generatedTest/java/\"");
			System.exit(1);
		}

		boolean validate;
		if ("true".equalsIgnoreCase(args[0]) || "false".equalsIgnoreCase(args[0])) {
			validate = Boolean.parseBoolean(args[0]);
		}
		else {
			System.err.println("The first argument VALIDATE should be true or false");
			System.exit(1);
			return;
		}

		String srcPath = args[1];
		String generatedSrcPath = args[2]; // "src/generated/";
		String testPath = args[3]; // "src/test/";
		String generatedTestPath = args[4]; // "src/generatedTest/";

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

		boolean multiTenant = false;
		if (args.length >= 7) {
			if ("true".equalsIgnoreCase(args[6]) || "false".equalsIgnoreCase(args[6])) {
				multiTenant = Boolean.parseBoolean(args[6]);
			}
			else {
				System.err.println("The seventh argument MULTI-TENANT should be true or false");
				System.exit(1);
			}
		}

		boolean debug = false;
		if (args.length >= 8) {
			if ("true".equalsIgnoreCase(args[7]) || "false".equalsIgnoreCase(args[7])) {
				debug = Boolean.parseBoolean(args[7]);
			}
			else {
				System.err.println("The eighth argument DEBUG should be true or false");
				System.exit(1);
			}
		}

		String[] excludedModules = null;
		if ((args.length >= 9) && (args[8] != null) && (! args[8].isEmpty())) {
			excludedModules = args[8].split(",");
		}
		
		String customisationsClassName = null;
		if ((args.length >= 10 )&& (args[9] != null) && (! args[9].isEmpty())) {
			customisationsClassName = args[9];
		}
		
		DomainGenerator.registerCustomisations(customisationsClassName);
		DomainGenerator.generate(validate, debug, multiTenant, dialectOptions, srcPath, generatedSrcPath, testPath, generatedTestPath, excludedModules);
	}
}
