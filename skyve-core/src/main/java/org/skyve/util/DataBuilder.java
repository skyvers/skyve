package org.skyve.util;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.lang.reflect.Method;
import java.security.SecureRandom;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.UUID;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.GeometryFactory;
import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.ChildBean;
import org.skyve.domain.types.Decimal;
import org.skyve.domain.types.Decimal2;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.AssociationImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.model.document.field.Decimal10;
import org.skyve.impl.metadata.model.document.field.Decimal5;
import org.skyve.impl.metadata.model.document.field.LengthField;
import org.skyve.impl.metadata.model.document.field.LongInteger;
import org.skyve.impl.metadata.model.document.field.Text;
import org.skyve.impl.metadata.model.document.field.TextFormat;
import org.skyve.impl.metadata.model.document.field.validator.DecimalValidator;
import org.skyve.impl.metadata.model.document.field.validator.IntegerValidator;
import org.skyve.impl.metadata.model.document.field.validator.LongValidator;
import org.skyve.impl.metadata.model.document.field.validator.TextValidator.ValidatorType;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.Attribute.UsageType;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.metadata.model.document.Collection;
import org.skyve.metadata.model.document.Collection.CollectionType;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.DomainType;
import org.skyve.metadata.model.document.Reference;
import org.skyve.metadata.model.document.Relation;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.util.test.DataMap;
import org.skyve.util.test.SkyveFactory;
import org.skyve.util.test.SkyveFixture;
import org.skyve.util.test.SkyveFixture.FixtureType;

import com.mifmif.common.regex.Generex;

/**
 * <p>
 * DataBuilder is used to create random instances of domain objects generated from Skyve document definitions.
 * It follows the builder pattern and has options for including and excluding both scalar (single valued) attributes
 * and references (associations and collections).
 * The attributes are populated with data that conforms to the data types, lengths and constraints
 * declared in the Document metadata.
 * DataBuilder can be recursive enabling the instantiation and populate of an entire object tree graph from 1 starting point.
 * </p>
 * 
 * <p>
 * Data Factories and Fixtures are also catered for.
 * By convention, a document can have a corresponding factory by defining a class called <Document-Name>Factory, similar to
 * Bizlets. DataBuilder will find these classes when it needs to construct an instance of the document.
 * It looks for public static or instance methods that take no arguments and returns the domain object type required.
 * If there is more than one candidate method that can be called, DataBuilder will randomly call one of the methods.
 * </p>
 * 
 * <p>
 * Fixtures are named groupings of methods that when executed together can collaboratively produce a data set for a specific
 * purpose. Fixtures can be named with a String name, or there are implicit fixture types that are defined in the FixtureType
 * enum. Annotating the methods in a Data Factory with either a fixture name or fixture type acts like a filter ensuring
 * that only suitable methods for each fixture (or use case) are called. Methods can be given a combination of multiple fixture
 * names and types. DataBuilders and the SAIL language can name the fixture to use when generating data in this manner.
 * A Data Factory can enlist the help of a DataBuilder in its fixture methods, but beware of infinite recursion problems.
 * Recursion is usually ended by ensuring that a different fixture or no fixture is called from the calling fixture method.
 * </p>
 * 
 * <p>
 * The fixture types set the starting state of the DataBuilder to something that should be useful for the given use case.
 * For instance, SAIL sets no recursion and population of all attributes but CRUD sets infinite recursion and no optional
 * references.
 * </p>
 * 
 * <pre>
 * Usage:
 * Create an object for a CRUD test
 * <code>
 * Contact c = new DataBuilder().fixture(FixtureType.crud).build(Contact.MODULE_NAME, Contact.DOCUMENT_NAME);
 * </code>
 * </pre>
 * 
 * <p>
 * DataBuilder uses the current user and customer (CORE.getUser()) during creation of random data.
 * </p>
 * 
 * <p>
 * There are a number of methods that can be called in the DataBuilder before calling one of the build methods
 * that filter and configure behavior for the different attributes. Check the method javadoc.
 * </p>
 * 
 * <p>
 * NB - Should improve the cardinality(), name() & depth() to allow complex binding expressions to reach
 * through the recursive calls down the object graph.
 * </p>
 * 
 * @author mike
 */
public class DataBuilder {
	private static final SecureRandom RANDOM = new SecureRandom();
	static {
		RANDOM.setSeed(RANDOM.generateSeed(20));
	}

	private static final String NUMBERS = "0123456789";
	private static final String LETTERS = "abcdefghijklmnopqrstuvwxyz";
	private static final String ALPHA_NUMERIC = LETTERS + NUMBERS;

	/**
	 * Name of generic text file to fill long text fields and memos.
	 */
	private static final String LOREM = "lorem.txt";

	/**
	 * Cache of module.document.attributeName to loaded file random values.
	 */
	private static final Map<String, List<String>> DATA_CACHE = new HashMap<>();

	/**
	 * Cache of module.document.attributeName to @DataMap fileNames.
	 */
	private static final Map<String, String> DATA_MAP_CACHE = new HashMap<>();

	private User user;
	private Customer customer;
	private String fixture;
	private boolean requiredScalars = true;
	private boolean optionalScalars = true;
	private boolean requiredReferences = true;
	private boolean optionalReferences = true;
	private boolean persistent = true;
	private boolean transients = true;
	private boolean view = true;
	private boolean domain = true;
	private boolean deprecated = true;
	private Map<AttributeType, Boolean> types = null;
	private Map<String, Boolean> names = null;
	private Map<String, Integer> cardinalities = null;
	private int depth = 0;
	private Map<String, Integer> depths = null;
	private Set<String> visitedBizIds = null;

	public DataBuilder() {
		user = CORE.getUser();
		customer = user.getCustomer();
	}
	
	/**
	 * Set a fixture that this builder will build for.
	 * @param fixture	The name of the fixture
	 * @return
	 */
	public DataBuilder fixture(@SuppressWarnings("hiding") String fixture) {
		this.fixture = fixture;
		return this;
	}

	/**
	 * Set a fixture that this build will build for.
	 * This method will configure the builder in the most useful way given the fixture type.
	 * @param type	The implicit fixture type.
	 * @return
	 */
	public DataBuilder fixture(FixtureType type) {
		if (FixtureType.crud.equals(type)) {
			depth = Integer.MAX_VALUE;
			optionalReferences = false;
		}
		else if (FixtureType.sail.equals(type)) {
			depth = 0;
		}
		this.fixture = type.toString();
		return this;
	}

	/**
	 * Include or exclude required attributes (scalar and references)
	 * @param include
	 * @return
	 */
	public DataBuilder required(boolean include) {
		requiredScalars = include;
		requiredReferences = include;
		return this;
	}
	
	/**
	 * Include or exclude required scalar and/or references independently
	 * @param includeScalars
	 * @param includeReferences
	 * @return
	 */
	public DataBuilder required(boolean includeScalars, boolean includeReferences) {
		requiredScalars = includeScalars;
		requiredReferences = includeReferences;
		return this;
	}

	/**
	 * Include or exclude optional attributes (scalar and references)
	 * @param include
	 * @return
	 */
	public DataBuilder optional(boolean include) {
		optionalScalars = include;
		optionalReferences = include;
		return this;
	}

	/**
	 * Include or exclude optional scalar and/or references independently
	 * @param includeScalars
	 * @param includeReferences
	 * @return
	 */
	public DataBuilder optional(boolean includeScalars, boolean includeReferences) {
		optionalScalars = includeScalars;
		optionalReferences = includeReferences;
		return this;
	}

	/**
	 * Include or exclude persistent attributes.
	 * @param include
	 * @return
	 */
	public DataBuilder persistent(boolean include) {
		persistent = include;
		return this;
	}
	
	/**
	 * Include or exclude transient attributes
	 * @param include
	 * @return
	 */
	public DataBuilder transients(boolean include) {
		this.transients = include;
		return this;
	}
	
	/**
	 * Include or exclude view attributes
	 * @param include
	 * @return
	 */
	public DataBuilder view(boolean include) {
		this.view = include;
		return this;
	}
	
	/**
	 * Include or exclude domain attributes
	 * @param include
	 * @return
	 */
	public DataBuilder domain(boolean include) {
		this.domain = include;
		return this;
	}

	/**
	 * Include or exclude deprecated attributes
	 * @param include
	 * @return
	 */
	public DataBuilder deprecated(boolean include) {
		this.deprecated = include;
		return this;
	}

	/**
	 * Include or exclude attributes by their attribute type
	 * @param type
	 * @param include
	 * @return
	 */
	public DataBuilder type(AttributeType type, boolean include) {
		if (types == null) {
			types = new HashMap<>();
		}
		types.put(type, Boolean.valueOf(include));
		return this;
	}

	/**
	 * Include or exclude attributes by their name.
	 * NB This is not bindings.
	 * @param name
	 * @param include
	 * @return
	 */
	public DataBuilder name(String name, boolean include) {
		if (names == null) {
			names = new HashMap<>();
		}
		names.put(name, Boolean.valueOf(include));
		return this;
	}

	/**
	 * Specify how many elements of a collection to create.
	 * @param binding
	 * @param cardinality
	 * @return
	 */
	public DataBuilder cardinality(String binding, int cardinality) {
		if (cardinalities == null) {
			cardinalities = new HashMap<>();
		}
		cardinalities.put(binding, Integer.valueOf(cardinality));
		return this;
	}
	
	/**
	 * Specify an overall recursion depth for the build
	 * @param depth
	 * @return
	 */
	public DataBuilder depth(@SuppressWarnings("hiding") int depth) {
		this.depth = depth;
		return this;
	}
	
	/**
	 * Specify the recursion depth for an association or collection.
	 * 
	 * @param binding
	 * @param depth
	 * @return
	 */
	public DataBuilder depth(String binding, @SuppressWarnings("hiding") int depth) {
		if (depths == null) {
			depths = new HashMap<>();
		}
		depths.put(binding, Integer.valueOf(depth));
		return this;
	}

	/**
	 * Build a domain bean.
	 * @param moduleName
	 * @param documentName
	 * @return
	 */
	public <T extends Bean> T build(String moduleName, String documentName) {
		Module m = customer.getModule(moduleName);
		Document d = m.getDocument(customer, documentName);
		initialiseCycleTracking();
		return build(m, d, 0);
	}
	
	/**
	 * Build a domain bean.
	 * @param module
	 * @param document
	 * @return
	 */
	public <T extends Bean> T build(Module module, Document document) {
		initialiseCycleTracking();
		return build(module, document, 0);
	}
	
	/**
	 * Build a domain bean.
	 * @param document
	 * @return
	 */
	public <T extends Bean> T build(Document document) {
		Module m = customer.getModule(document.getOwningModuleName());
		initialiseCycleTracking();
		return build(m, document, 0);
	}

	private void initialiseCycleTracking() {
		visitedBizIds = new TreeSet<>();
	}
	
	/**
	 * The guts of the build function
	 * @param module
	 * @param document
	 * @param currentDepth
	 * @return
	 */
	private synchronized <T extends Bean> T build(Module module,
													Document document,
													int currentDepth) {
		T result = null;
		try {
			result = dataFactory((DocumentImpl) document);
			if (result == null) {
				result = randomBean(module, document);

				String resultBizId = result.getBizId();
				if (visitedBizIds.contains(resultBizId)) {
					return result;
				}
				visitedBizIds.add(resultBizId);

				for (Attribute attribute : document.getAllAttributes()) {
					if (filter(attribute)) {
						continue;
					}
					
					String name = attribute.getName();
					AttributeType type = attribute.getAttributeType();
					if (AttributeType.association.equals(type)) {
						if (currentDepth < depth) { // check global depth
							// check assigned depth
							if (depths != null) {
								Integer associationDepth = depths.get(name);
								if ((associationDepth != null) && (currentDepth >= associationDepth.intValue())) {
									continue;
								}
							}
		
							AssociationImpl association = (AssociationImpl) attribute;
							Module associationModule = module;
							String associationModuleRef = module.getDocumentRefs().get(association.getDocumentName()).getReferencedModuleName();
							if (associationModuleRef != null) {
								associationModule = customer.getModule(associationModuleRef);
							}
							Document associationDocument = associationModule.getDocument(customer, association.getDocumentName());
							BindUtil.set(result,
											name,
											build(associationModule, associationDocument, currentDepth + 1));
						}
					}
					else if (AttributeType.collection.equals(type)) {
						if (currentDepth < depth) { // check global depth
							// check assigned depth
							if (depths != null) {
								Integer collectionDepth = depths.get(name);
								if ((collectionDepth != null) && (currentDepth >= collectionDepth.intValue())) {
									continue;
								}
							}
		
							Collection collection = (Collection) attribute;
							Integer minCardinality = collection.getMinCardinality();
							Module collectionModule = module;
							String collectionModuleRef = module.getDocumentRefs().get(collection.getDocumentName()).getReferencedModuleName();
							if (collectionModuleRef != null) {
								collectionModule = customer.getModule(collectionModuleRef);
							}
							Document collectionDocument = collectionModule.getDocument(customer, collection.getDocumentName());
							boolean childCollection = CollectionType.child.equals(collection.getType()) &&
														document.getName().equals(collectionDocument.getParentDocumentName());
							@SuppressWarnings("unchecked")
							List<Bean> list = (List<Bean>) BindUtil.get(result, name);
							
							int cardinality = 1; // default to a single element
							// Set min cardinality if it is set on the met-data and is greater than 2
							if ((minCardinality != null) && (minCardinality.intValue() > cardinality)) {
								cardinality = minCardinality.intValue();
							}
							
							// check if there is a cardinality set by the build for this collection
							if (cardinalities != null) {
								Integer collectionCardinality = cardinalities.get(name);
								if (collectionCardinality != null) {
									cardinality = collectionCardinality.intValue();
								}
							}
		
							for (int i = 0; i < cardinality; i++) {
								Bean element = build(collectionModule, collectionDocument, currentDepth + 1);
								list.add(element);
								if (childCollection) {
									@SuppressWarnings("unchecked")
									ChildBean<Bean> child = (ChildBean<Bean>) element;
									child.setParent(result);
								}
							}
						}
					}
				}
			}
		}
		catch (Exception e) {
			throw new MetaDataException(String.format("Could not build an instance of %s.%s", module.getName(), document.getName()), e);
		}
		
		return result;
	}
	/**
	 * Create and assign random scalar data.
	 * @param module
	 * @param document
	 * @return
	 * @throws Exception
	 */
	
	@SuppressWarnings("incomplete-switch")
	private <T extends Bean> T randomBean(Module module, Document document)
	throws Exception {
		T result = document.newInstance(user);
		
		for (Attribute attribute : document.getAllAttributes()) {
			if ((attribute instanceof Relation) || (filter(attribute))) {
				continue;
			}

			String name = attribute.getName();
			AttributeType type = attribute.getAttributeType();
			String domainValue = randomDomainValue(customer, document, attribute, result);
			if (domainValue != null) {
				if (! domainValue.isEmpty()) {
					BindUtil.convertAndSet(result, name, domainValue);
				}
			}
			else {
				switch (type) {
					case bool:
						// Random bools always are set to false as most processing changes around the true value.
						// This is considered the standard case, and can be set true after the random instance is constructed if needed.
						BindUtil.set(result, name, Boolean.FALSE);
						break;
					case colour:
						BindUtil.set(result, name, "#FFFFFF");
						break;
					case date:
					case dateTime:
					case time:
					case timestamp:
						BindUtil.convertAndSet(result, name, new Date());
						break;
					case decimal10:
					case decimal2:
					case decimal5:
						BindUtil.convertAndSet(result, name, randomDecimal(attribute));
						break;
					case integer:
					case longInteger:
						BindUtil.convertAndSet(result, name, randomInteger(attribute));
						break;
					case enumeration:
						// pick a random value from the enum
						@SuppressWarnings("unchecked")
						Class<Enum<?>> clazz = (Class<Enum<?>>) Binder.getPropertyType(result, name);
						BindUtil.set(result, name, randomEnum(clazz, null));
						break;
					case geometry:
						BindUtil.set(result, name, new GeometryFactory().createPoint(new Coordinate(RANDOM.nextInt(180) - RANDOM.nextInt(180), RANDOM.nextInt(90) - RANDOM.nextInt(90))));
						break;
					case id:
						BindUtil.set(result, name, UUID.randomUUID().toString());
						break;
					case markup:
					case memo:
					case text:
						BindUtil.set(result, name, randomText(user.getCustomerName(), module, document, attribute));
						break;
				}
			}
		}

		return result;
	}
	
	/**
	 * Filter out the attributes based on the builder criteria.
	 * @param attribute
	 * @return
	 */
	private boolean filter(Attribute attribute) {
		String name = attribute.getName();
		AttributeType type = attribute.getAttributeType();

		if (attribute.isRequired()) {
			if (attribute instanceof Reference) {
				if (! requiredReferences) {
					return true;
				}
			}
			else {
				if (! requiredScalars) {
					return true;
				}
			}
		}
		else {
			if (attribute instanceof Reference) {
				if (! optionalReferences) {
					return true;
				}
			}
			else {
				if (! optionalScalars) {
					return true;
				}
			}
		}
		if (attribute.isPersistent()) {
			if (! persistent) {
				return true;
			}
		}
		else {
			if (! transients) {
				return true;
			}
		}
		UsageType usage = attribute.getUsage();
		if (UsageType.view.equals(usage)) {
			if (! view) {
				return true;
			}
		}
		if (UsageType.domain.equals(usage)) {
			if (! domain) {
				return true;
			}
		}
		if (attribute.isDeprecated()) {
			if (! deprecated) {
				return true;
			}
		}
		if (types != null) {
			if (Boolean.FALSE.equals(types.get(type))) {
				return true;
			}
		}
		if (names != null) {
			if (Boolean.FALSE.equals(names.get(name))) {
				return true;
			}
		}
		
		return false;
	}
	
	/**
	 * Get domain values for a document attribute if they exist
	 * @return null if there are no domain values or "" if there is a domain value set defined but no yielded values
	 */
	private static String randomDomainValue(Customer customer, Document document, Attribute attribute, Bean bean) {
		String result = null;

		DomainType domainType = attribute.getDomainType();
		if (domainType != null) {
			List<DomainValue> values = ((DocumentImpl) document).getDomainValues((CustomerImpl) customer, domainType, attribute, bean, true);
			if ((values != null) && (! values.isEmpty())) {
				result = values.get(RANDOM.nextInt(values.size())).getCode();
			}
			else {
				result = "";
			}
		}
		
		return result;
	}
	
	@SuppressWarnings("unchecked")
	private <T extends Bean> T dataFactory(DocumentImpl document)
	throws Exception {
		T result = null;
		
		// If we have a data factory defined for this document,
		Object factory = CORE.getRepository().getDataFactory(customer, document.getOwningModuleName(), document.getName());
		if (factory != null) {
			Class<?> beanType = document.getBeanClass(customer);
			List<Method> methods = new ArrayList<>();
			if (fixture != null) {
				// If we have a fixture and the factory has no argument methods with a return type of the bean
				// for the fixture then pick one of those
				for (Method method : factory.getClass().getMethods()) {
					SkyveFixture annotation = method.getAnnotation(SkyveFixture.class);
					if (annotation != null) {
						FixtureType[] methodTypes = annotation.types();
						if (methodTypes != null) {
							for (FixtureType methodType : methodTypes) {
								if (fixture.equals(methodType.toString()) && 
										(method.getParameterCount() == 0) &&
										beanType.isAssignableFrom(method.getReturnType())) {
									methods.add(method);
								}
							}
						}
						String[] methodNames = annotation.names();
						if (methodNames != null) {
							for (String methodName : methodNames) {
								if (fixture.equals(methodName) && 
										(method.getParameterCount() == 0) &&
										beanType.isAssignableFrom(method.getReturnType())) {
									methods.add(method);
								}
							}
						}
					}
				}
			}
			
			if (methods.isEmpty()) {
				// get all methods without a fixture annotation 
				// with no arguments and a return type of the bean and pick one of those
				for (Method method : factory.getClass().getMethods()) {
					if ((! method.isAnnotationPresent(SkyveFixture.class)) && 
							(method.getParameterCount() == 0) && 
							beanType.isAssignableFrom(method.getReturnType())) {
						methods.add(method);
					}
				}
			}
			
			// If there are any valid methods at all
			if (! methods.isEmpty()) {
				result = (T) methods.get(RANDOM.nextInt(methods.size())).invoke(factory);
			}
		}
		
		return result;
	}
	
	/**
	 * Creates a cache key for an attribute so it is unique per document.
	 */
	private static String attributeKey(final Module module, final Document document, final String attributeName) {
		if (attributeName != null) {
			if (module != null && document != null) {
				return String.format("%s.%s.%s", module.getName(), document.getName(), attributeName);
			}
			return attributeName;
		}
		return null;
	}

	/**
	 * Checks if the requested filename has a file extension.
	 * @param filename The name of the file to check
	 * @return True if the string contains a period followed by at least one character, false otherwise
	 */
	private static boolean hasExtension(final String filename) {
		if(filename != null && filename.length() > 0 && filename.indexOf(".") > 0) {
			if ((filename.substring(filename.indexOf(".") + 1)).length() > 0) {
				return true;
			}
		}
		return false;
	}

	private static String randomEmail(int length) {
		int addressLength = (int) Math.floor((length - 2) / 2);
		int domainLength = (int) Math.floor((length - 2) / 2) - 2;

		char[] address = new char[addressLength];
		for (int i = 0; i < addressLength; i++) {
			address[i] = Character.toChars(65 + (int) (Math.random() * 26))[0];
		}

		char[] domain = new char[domainLength];
		for (int i = 0; i < domainLength; i++) {
			domain[i] = Character.toChars(65 + (int) (Math.random() * 26))[0];
		}

		char[] code = new char[2];
		for (int i = 0; i < 2; i++) {
			code[i] = Character.toChars(65 + (int) (Math.random() * 26))[0];
		}

		return String.valueOf(address) + "@" + String.valueOf(domain) + "." + String.valueOf(code);
	}

	/**
	 * Returns a random value from the enum class
	 * 
	 * @param clazz The enum class
	 * @param currentValue The current int value of the enum so that it is not chosen again
	 * @return A random enum constant
	 */
	private static <T extends Enum<?>> T randomEnum(Class<T> clazz, Integer currentValue) {
		int x;
		if (currentValue != null) {
			int currentValueInt = currentValue.intValue();
			do {
				x = RANDOM.nextInt(clazz.getEnumConstants().length);
			} while (x == currentValueInt);
		} else {
			x = RANDOM.nextInt(clazz.getEnumConstants().length);
		}

		return clazz.getEnumConstants()[x];
	}

	/**
	 * Returns a random decimal which conforms to any min and max validators set,
	 * otherwise between 0 and 10,000.
	 * 
	 * @param attribute The attribute to generate the random decimal for
	 * @return A random decimal
	 */
	private static Decimal randomDecimal(Attribute attribute) {
		Decimal min = new Decimal2(0), max = new Decimal2(10000);

		DecimalValidator validator = null;
		if (attribute instanceof org.skyve.impl.metadata.model.document.field.Decimal2) {
			org.skyve.impl.metadata.model.document.field.Decimal2 field = (org.skyve.impl.metadata.model.document.field.Decimal2) attribute;
			validator = field.getValidator();
		} else if (attribute instanceof Decimal5) {
			Decimal5 field = (Decimal5) attribute;
			validator = field.getValidator();
		} else if (attribute instanceof Decimal10) {
			Decimal10 field = (Decimal10) attribute;
			validator = field.getValidator();
		}

		if (validator != null) {
			if (validator.getMin() != null) {
				min = validator.getMin();
			}
			if (validator.getMax() != null) {
				max = validator.getMax();
			}
		}

		return new Decimal2(RANDOM.nextInt(
				(max.subtract(min))
						.add(new Decimal2(1)).intValue())).add(min);
	}

	/**
	 * Returns a random string which complies to the format mask
	 * of the text attribute
	 * 
	 * @param textFormat The format to comply to
	 * @return A format compliant random string
	 */
	private static String randomFormat(TextFormat textFormat) {

		String mask = textFormat.getMask();
		String out = new String();

		for (int i = 0; i < mask.length(); i++) {
			char c = mask.charAt(i);
			switch (c) {
				case '#':
					out += NUMBERS.charAt(RANDOM.nextInt(NUMBERS.length()));
					break;
				case 'A':
					out += ALPHA_NUMERIC.charAt(RANDOM.nextInt(ALPHA_NUMERIC.length()));
					break;
				case 'L':
					out += LETTERS.charAt(RANDOM.nextInt(LETTERS.length()));
					break;
				default:
					out += c;
					break;
			}
		}

		return out;
	}

	/**
	 * Returns a random number which conforms to any min and max validators set,
	 * otherwise between 0 and 10,000.
	 * 
	 * @param attribute The attribute to generate the random integer for
	 * @return A random integer
	 */
	private static Integer randomInteger(Attribute attribute) {
		int min = 0, max = 10000;

		// if there is a min and max make sure it is within the range
		if (attribute instanceof org.skyve.impl.metadata.model.document.field.Integer) {
			org.skyve.impl.metadata.model.document.field.Integer field = (org.skyve.impl.metadata.model.document.field.Integer) attribute;
			IntegerValidator validator = field.getValidator();
			if (validator != null) {
				if (validator.getMin() != null) {
					min = validator.getMin().intValue();
				}
				if (validator.getMax() != null) {
					max = validator.getMax().intValue();
				}
			}
		} else if (attribute instanceof org.skyve.impl.metadata.model.document.field.LongInteger) {
			LongInteger field = (LongInteger) attribute;
			LongValidator validator = field.getValidator();
			if (validator != null) {
				if (validator.getMin() != null) {
					min = validator.getMin().intValue();
				}
				if (validator.getMax() != null) {
					max = validator.getMax().intValue();
				}
			}
		}

		return new Integer(RANDOM.nextInt((max - min) + 1) + min);
	}

	/**
	 * Returns a random string which complies to the regular
	 * expression of the text attribute. Returns null if this
	 * cannot be achieved.
	 * 
	 * @param regularExpression The regular expression to comply to
	 * @return A regex compliant random string, or null
	 */
	private static String randomRegex(String regularExpression) {
		Generex generex = new Generex(regularExpression);
		// Generate random String matching the regex
		try {
			String result = generex.random();
			// strip boundaries
			if (result.startsWith("^") && result.endsWith("$")) {
				return StringUtils.substringBetween(result, "^", "$");
			}
			return result;
		}
		catch (@SuppressWarnings("unused") Exception e) {
			Util.LOGGER.warning("Couldnt generate compliant string for expression " + regularExpression);
		}
		return null;
	}

	private static String randomString(int length) {
		char[] guts = new char[length];
		for (int i = 0; i < length; i++) {
			guts[i] = Character.toChars(65 + (int) (Math.random() * 26))[0];
		}

		return String.valueOf(guts);
	}

	/**
	 * Constructs a random string for the specified {@link Text} attribute. It will attempt
	 * to fill the text based on:
	 * <ul>
	 * <li>the presence of a file with the attribute name, e.g. firstName.txt
	 * <li>the presence of a format mask
	 * <li>a regular expression or other validator
	 * <li>random text
	 * 
	 * @param module The module this attribute belongs to
	 * @param document The document this attribute belongs to
	 * @param text The attribute to create the random data for
	 * @return A string containing random data for the text attribute
	 * @throws IOException
	 */
	private static String randomText(String customerName, Module module, Document document, Attribute attribute) throws IOException {
		if (attribute != null) {
			String fileName = null;
			Integer length = null;

			// check if there is a data map for this field
			if (module != null && document != null) {
				final String key = attributeKey(module, document, attribute.getName());
				if (DATA_MAP_CACHE.containsKey(key)) {
					fileName = DATA_MAP_CACHE.get(key);
					Util.LOGGER.fine(String.format("Loaded %s filename from cache", key));
				} else {
					String className = String.format("modules.%1$s.%2$s.%2$sFactory", module.getName(), document.getName());
					Util.LOGGER.fine("Looking for factory class " + className);
					try {
						Class<?> c = Thread.currentThread().getContextClassLoader().loadClass(className);
						if (c != null) {
							Util.LOGGER.fine("Found class " + c.getName());
							if (c.isAnnotationPresent(DataMap.class)) {
								DataMap annotation = c.getAnnotation(DataMap.class);
								Util.LOGGER.fine(
										String.format("attributeName: %s fileName: %s", annotation.attributeName(),
												annotation.fileName()));
								if (attribute.getName().equals(annotation.attributeName())) {
									fileName = annotation.fileName();
									DATA_MAP_CACHE.put(key, fileName);
								}
							} else if (c.isAnnotationPresent(SkyveFactory.class)) {
								SkyveFactory annotation = c.getAnnotation(SkyveFactory.class);
								DataMap[] values = annotation.value();
								for (DataMap map : values) {
									Util.LOGGER.fine(
											String.format("attributeName: %s fileName: %s", map.attributeName(), map.fileName()));
									if (attribute.getName().equals(map.attributeName())) {
										fileName = map.fileName();
										DATA_MAP_CACHE.put(key, fileName);
										break;
									}
								}
							}
						}
					}
					catch (@SuppressWarnings("unused") Exception e) {
						// couldn't find the extension file on the classpath
					}	
				}

				// check if there is a data file for this field
				Util.LOGGER.fine(String.format(
						"Looking for test data file in data/%s.txt", fileName != null ? fileName : attribute.getName()));
				String value = randomValueFromFile(customerName, module, document, attribute.getName(), fileName);
				if (value != null) {
					Util.LOGGER.fine(String.format("Random %s: %s", attribute.getName(), value));
					return value;
				}
			}

			// check if this string has a format mask
			if(attribute instanceof Text) {
				Text text = (Text) attribute;
				length = Integer.valueOf(text.getLength());
			
				if (text.getFormat() != null) {
					// check if it has a format mask and a regex, if so, prefer the regex
					if (text.getValidator() != null && text.getValidator().getRegularExpression() != null
							&& text.getValidator().getType() == null) {
						// return text matching the regex
						String xeger = randomRegex(text.getValidator().getRegularExpression());
						if (xeger != null) {
							return xeger;
						}
					}

					// return text matching the format mask
					return randomFormat(text.getFormat());
				} else if (text.getValidator() != null && text.getValidator().getRegularExpression() != null
						&& text.getValidator().getType() == null) {
					// check if this string has a regex and no validator type
					String xeger = randomRegex(text.getValidator().getRegularExpression());
					if (xeger != null) {
						return xeger;
					}
				} else {
					// check if this is an email address
					if (text.getValidator() != null && ValidatorType.email.equals(text.getValidator().getType())) {
						return randomEmail(((LengthField) text).getLength());
					} else if (text.getValidator() != null && text.getValidator().getRegularExpression() != null) {
						// check if this string has a regex via a validator type
						String xeger = randomRegex(text.getValidator().getRegularExpression());
						if (xeger != null) {
							return xeger;
						}
					}
				}
			}
			
			// return random lorem ipsum text
			if (length == null) {
				// set an arbitrary max length for memo fields
				length = Integer.valueOf(2048);
			}
			String value = randomValueFromFile(null, null, null, LOREM);
			if (value != null) {
				String[] sentences = value.split("\\.");
				shuffleArray(sentences);
				int i = 0,
						min = length.intValue() / 3,
						r = RANDOM.nextInt(length.intValue() + 1 - min) + min;

				// keep adding sentences until we hit the length
				StringBuilder b = new StringBuilder();
				while (b.length() < r) {
					b.append(sentences[i]).append(".");
					i++;
					if (b.length() > r) {
						String out = b.toString();
						out = out.trim().substring(0, r).trim();
						if (out.indexOf(".") > 0) {
							// trim to last sentence boundary
							out = out.substring(0, out.lastIndexOf(".") + 1).trim();
						}
						if (out.length() > 0) {
							Util.LOGGER.fine(String.format("Random %s for %s with length %d(%d): %s",
									attribute.getAttributeType(),
									attribute.getName(),
									Integer.valueOf(r),
									length,
									out));
							return out;
						}
					}
				}

			}

			// return random text
			return randomString(length.intValue());
		}

		return null;
	}

	/**
	 * <p>
	 * Returns a random value from the test data file for the specified attribute if
	 * a data file exists, null otherwise. The file name is expected to be within a
	 * <code>data</code> directory on the classpath with the same name (case sensitive)
	 * as the attribute name.
	 * </p>
	 * 
	 * <p>
	 * E.g. <code>src/test/resources/data/postCode.txt</code>
	 * </p>
	 * 
	 * <p>
	 * The file will be cached the first time it is requested, and loaded from memory
	 * for subsequent random value requests.
	 * </p>
	 * 
	 * @param module The module this attribute belongs to
	 * @param document The document this attribute belongs to
	 * @param attributeName The attribute name to return the random value for
	 * @param fileName <em>Optional</em> The filename to load random values for, if it doesn't match the attribute name
	 * @return A random value from the data file if it exists, null otherwise
	 * @throws IOException
	 */
	private static String randomValueFromFile(String customerName, final Module module, final Document document, final String attributeName,
			final String... fileName) throws IOException {
		if (attributeName != null) {
			final String key = attributeKey(module, document, attributeName);

			List<String> values = null;
			if (DATA_CACHE.containsKey(key)) {
				values = DATA_CACHE.get(key);
				Util.LOGGER.fine(String.format("Loaded %s list from cache", key));
			} else {
				String fileToLoad = attributeName;
				if (fileName != null && fileName.length == 1 && fileName[0] != null) {
					fileToLoad = fileName[0];
				}

				// default the extension if none specified
				if (fileToLoad != null && !hasExtension(fileToLoad)) {
					fileToLoad = fileToLoad + ".txt";
				}

				Util.LOGGER.fine("Attempting to find on the classpath: " + String.format("data/%s", fileToLoad));
				File file = CORE.getRepository().findResourceFile(String.format("data/%s",
																					fileToLoad),
																					customerName,
																					(module == null) ? null : module.getName());
				if ((file != null) && file.exists()) {
					try (InputStream inputStream = new FileInputStream(file)) {
						values = readFromInputStream(inputStream);
						DATA_CACHE.put(key, values);
						Util.LOGGER.fine(String.format("Caching attribute %s with filename %s", key, fileToLoad));
						if (values != null && values.size() > 0) {
							Util.LOGGER.fine(String.format("Loaded %s list from %s. Found %d values.", attributeName, fileToLoad,
									Integer.valueOf(values.size())));
						}
					}
				}
			}

			if (values != null) {
				// return random value or all of lorem
				if (attributeName.equals(LOREM)) {
					return values.stream().collect(Collectors.joining("\n"));
				}
				return values.get(RANDOM.nextInt(values.size()));
			}
		}

		return null;
	}

	/**
	 * Attempts to read a test data file from an input stream and stores each line
	 * as a string in a list.
	 * 
	 * @param inputStream The input stream to read from
	 * @return A list of strings for each line in the file, null if the input stream cannot be read
	 * @throws IOException
	 */
	private static List<String> readFromInputStream(InputStream inputStream) throws IOException {
		if (inputStream == null) {
			return null;
		}
		List<String> list = new ArrayList<>();
		try (BufferedReader br = new BufferedReader(new InputStreamReader(inputStream))) {
			String line;
			while ((line = br.readLine()) != null) {
				list.add(line);
			}
		}
		return list;
	}

	/**
	 * Shuffles an array using the <a href="http://en.wikipedia.org/wiki/Fisher-Yates_shuffle">Fisher–Yates shuffle</a>.
	 * 
	 * @param arr The array to shuffle
	 */
	private static void shuffleArray(String[] arr) {
		for (int i = arr.length - 1; i > 0; i--) {
			int index = RANDOM.nextInt(i + 1);
			// Simple swap
			String a = arr[index];
			arr[index] = arr[i];
			arr[i] = a;
		}
	}
}
