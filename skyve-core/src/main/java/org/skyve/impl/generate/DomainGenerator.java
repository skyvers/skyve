package org.skyve.impl.generate;

import java.nio.file.Path;
import java.util.Arrays;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

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

	/**
	 * Array of Java reserved words. Used to checks if an attribute name is valid, e.g. <code>return</code>.
	 */
	public static final Set<String> JAVA_RESERVED_WORDS;

	/**
	 * Array of H2 database reserved words. Used to check if an attribute name is valid, e.g. <code>from</code>.
	 */
	public static final Set<String> H2_RESERVED_WORDS;

	/**
	 * Array of MySQL 5.x database reserved words. Used to check if an attribute name is valid, e.g. <code>from</code>.
	 */
	public static final Set<String> MYSQL_5_RESERVED_WORDS;

	/**
	 * Array of MySQL 8.x database reserved words. Used to check if an attribute name is valid, e.g. <code>from</code>.
	 */
	public static final Set<String> MYSQL_8_RESERVED_WORDS;

	/**
	 * Array of SqlServer T-SQL reserved words. Used to check if an attribute name is valid, e.g. <code>from</code>.
	 */
	public static final Set<String> SQL_SERVER_RESERVED_WORDS;

	/**
	 * Array of POSTGRESQL database reserved words. Used to check if an attribute name is valid, e.g. <code>from</code>.
	 */
	public static final Set<String> POSTGRESQL_RESERVED_WORDS;

	static {
		String javaReserved[] = {
				"abstract", "assert", "boolean", "break", "byte", "case",
				"catch", "char", "class", "const", "continue",
				"default", "do", "double", "else", "extends",
				"false", "final", "finally", "float", "for",
				"goto", "if", "implements", "import", "instanceof",
				"int", "interface", "long", "native", "new",
				"null", "package", "private", "protected", "public",
				"return", "short", "static", "strictfp", "super",
				"switch", "synchronized", "this", "throw", "throws",
				"transient", "true", "try", "void", "volatile", "while"
		};
		JAVA_RESERVED_WORDS = new TreeSet<>(Arrays.asList(javaReserved));

		String h2Reserved[] = {
				"all", "check", "constraint", "cross", "current_date", "current_time", "current_timestamp",
				"distinct", "except", "exists", "false", "fetch", "for", "foreign", "from", "full", "group",
				"having", "inner", "intersect", "is", "join", "like", "limit", "minus", "natural", "not", "null",
				"offset", "on", "order", "primary", "rownum", "select", "sysdate", "systime", "systimestamp",
				"today", "true", "union", "unique", "where", "with"
		};
		H2_RESERVED_WORDS = new TreeSet<>(Arrays.asList(h2Reserved));

		String mysql5Reserved[] = {
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
				"with", "write", "xor", "year_month", "generated"
		};
		MYSQL_5_RESERVED_WORDS = new TreeSet<>(Arrays.asList(mysql5Reserved));

		String mysql8ExtraReserved[] = {
				"array", "cume_dist", "dense_rank", "empty", "except", "first_value", "grouping", "groups",
				"json_table", "lag", "last_value", "lateral", "lead", "member", "nth_value", "ntile",
				"of", "over", "percent_rank", "rank", "recursive", "row_number", "system", "window"
		};
		MYSQL_8_RESERVED_WORDS = new TreeSet<>(MYSQL_5_RESERVED_WORDS);
		MYSQL_8_RESERVED_WORDS.addAll(Arrays.asList(mysql8ExtraReserved));
		String sqlServerReserved[] = {
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
				"precision", "execute", "primary", "within", "exists", "print", "exit", "proc"
		};
		SQL_SERVER_RESERVED_WORDS = new TreeSet<>(Arrays.asList(sqlServerReserved));

		String postgreSQLReserved[] = {
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
				"verbose", "when", "where"
		};
		POSTGRESQL_RESERVED_WORDS = new TreeSet<>(Arrays.asList(postgreSQLReserved));
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

	protected AbstractRepository repository;
	
	protected Map<Path, CharSequence> generation = new TreeMap<>();
	
	protected DomainGenerator(boolean write,
								boolean debug,
								boolean multiTenant,
								AbstractRepository repository,
								DialectOptions dialectOptions,
								String srcPath,
								String generatedSrcPath,
								String testPath,
								String generatedTestPath,
								String[] excludedModules) {
		this.write = write;
		this.debug = debug;
		this.multiTenant = multiTenant;
		this.repository = repository;
		this.dialectOptions = dialectOptions;
		this.srcPath = srcPath;
		this.generatedSrcPath = generatedSrcPath;
		this.testPath = testPath;
		this.generatedTestPath = generatedTestPath;
		this.excludedModules = excludedModules;
	}
	
	public void validate(String customerName) throws Exception {
		if (debug) System.out.println("Get customer " + customerName);
		Customer customer = repository.getCustomer(customerName);
		if (debug) System.out.println("Validate customer " + customerName);
		repository.validateCustomerForGenerateDomain(customer);
		for (Module module : customer.getModules()) {
			if (debug) System.out.println("Validate module " + module.getName());
			repository.validateModuleForGenerateDomain(customer, module);
			for (Entry<String, DocumentRef> entry : module.getDocumentRefs().entrySet()) {
				String documentName = entry.getKey();
				if (debug) System.out.println("Get document " + documentName);
				Document document = module.getDocument(customer, documentName);
				if (debug) System.out.println("Validate document " + documentName);
				repository.validateDocumentForGenerateDomain(customer, document);
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
					if (debug) System.out.println("Get edit view for document " + documentName + " and uxui " + uxuiName);
					View view = repository.getView(uxuiName, customer, document, ViewType.edit.toString());
					if (debug) System.out.println("Validate edit view for document " + documentName + " and uxui " + uxuiName);
					repository.validateViewForGenerateDomain(customer, document, view, uxuiName);
					view = repository.getView(uxuiName, customer, document, ViewType.create.toString());
					if (view != null) {
						if (debug) System.out.println("Validate create view for document " + documentName + " and uxui " + uxuiName);
						repository.validateViewForGenerateDomain(customer, document, view, uxuiName);
					}
				}
			}
		}
	}

	public abstract void generate() throws Exception;

	public static final DomainGenerator newDomainGenerator(boolean write,
															boolean debug,
															boolean multiTenant,
															AbstractRepository repository,
															DialectOptions dialectOptions,
															String srcPath,
															String generatedSrcPath,
															String testPath,
															String generatedTestPath,
															String... excludedModules) {
		return (UtilImpl.USING_JPA ? 
					new JPADomainGenerator(debug, multiTenant, repository, dialectOptions, srcPath, generatedSrcPath, testPath, generatedTestPath, excludedModules) : 
					new OverridableDomainGenerator(write, debug, multiTenant, repository, dialectOptions, srcPath, generatedSrcPath, testPath, generatedTestPath, excludedModules));
	}
	
	public static void generate(boolean debug,
									boolean multiTenant,
									DialectOptions dialectOptions,
									String srcPath,
									String generatedSrcPath,
									String testPath,
									String generatedTestPath,
									String... excludedModules)
	throws Exception {
		System.out.println("SRC PATH=" + srcPath);
		System.out.println("GENERATED SRC PATH=" + generatedSrcPath);
		System.out.println("TEST PATH=" + testPath);
		System.out.println("GENERATED TEST PATH=" + generatedTestPath);
		System.out.println("DIALECT OPTIONS=" + dialectOptions.toString());
		System.out.println("MULTI-TENANT=" + multiTenant);
		System.out.println("DEBUG=" + debug);
		System.out.println("EXCLUDED MODULES=" + ((excludedModules.length > 0) ? StringUtils.join(excludedModules, ", ") : ""));

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
		
		AbstractRepository repository = new LocalDesignRepository();
		DomainGenerator jenny = newDomainGenerator(true, debug, multiTenant, repository, dialectOptions, srcPath, generatedSrcPath, testPath, generatedTestPath, excludedModules);

		// generate for all customers
		for (String customerName : repository.getAllCustomerNames()) {
			jenny.validate(customerName);
		}
		jenny.generate();
	}
	
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

		String srcPath = args[0];
		String generatedSrcPath = args[1];// "src/generated/";
		String testPath = args[2];// "src/test/";
		String generatedTestPath = args[3];// "src/generatedTest/";

		DialectOptions dialectOptions = DialectOptions.H2_NO_INDEXES;
		if (args.length >= 5) {
			try {
				dialectOptions = DialectOptions.valueOf(args[4]);
			}
			catch (@SuppressWarnings("unused") IllegalArgumentException e) {
				System.err.println("The fifth argument DIALECT_OPTIONS should be one of the following "
						+ StringUtils.join(DialectOptions.values(), ", "));
				System.exit(1);
			}
		}

		boolean multiTenant = false;
		if (args.length >= 6) { // allow for multi-tenant mode if there are 6 arguments
			if ("true".equalsIgnoreCase(args[5]) || "false".equalsIgnoreCase(args[5])) {
				multiTenant = Boolean.parseBoolean(args[5]);
			}
			else {
				System.err.println("The sixth argument MULTI-TENANT should be true or false");
				System.exit(1);
			}
		}

		boolean debug = false;
		if (args.length >= 7) { // allow for debug mode if there are 4 arguments
			if ("true".equalsIgnoreCase(args[6]) || "false".equalsIgnoreCase(args[6])) {
				debug = Boolean.parseBoolean(args[6]);
			}
			else {
				System.err.println("The seventh argument DEBUG should be true or false");
				System.exit(1);
			}
		}

		String[] excludedModules = null;
		if (args.length == 8) {
			if ((args[7] != null) && (! args[7].isEmpty())) {
				excludedModules = args[7].split(",");
			}
		}
		DomainGenerator.generate(debug, multiTenant, dialectOptions, srcPath, generatedSrcPath, testPath, generatedTestPath, excludedModules);
	}
}
