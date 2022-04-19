package org.skyve.util;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Set;
import java.util.TreeSet;
import java.util.UUID;

import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.GeometryFactory;
import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.AssociationImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.Attribute.UsageType;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.metadata.model.document.Collection;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.DomainType;
import org.skyve.metadata.model.document.Reference;
import org.skyve.metadata.model.document.Relation;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.util.test.SkyveFixture;
import org.skyve.util.test.SkyveFixture.FixtureType;
import org.skyve.util.test.TestUtil;

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
	private static boolean trace = false;
	
	private static final Random RANDOM = new Random();

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
	 * Specify whether to log how the data was built.
	 * 
	 * @param trace
	 * @return
	 */
	public static void setTrace(boolean trace) {
		DataBuilder.trace = trace;
	}
	
	/**
	 * Build a domain bean.
	 * @param moduleName
	 * @param documentName
	 * @return
	 */
	public <T extends Bean> T build(String moduleName, String documentName) {
		return build(moduleName, documentName, true);
	}
	
	/**
	 * Build a domain bean within its Factory class. This will stop infinite recursion between the DataBuilder and the factory.
	 * @param moduleName
	 * @param documentName
	 * @return
	 */
	public <T extends Bean> T factoryBuild(String moduleName, String documentName) {
		return build(moduleName, documentName, false);
	}
	
	private <T extends Bean> T build(String moduleName, String documentName, boolean allowFactoryRecursion) {
		Module m = customer.getModule(moduleName);
		Document d = m.getDocument(customer, documentName);
		initialiseCycleTracking();
		return build(m, d, "", 0, allowFactoryRecursion);
	}
	
	/**
	 * Build a domain bean.
	 * @param module
	 * @param document
	 * @return
	 */
	public <T extends Bean> T build(Module module, Document document) {
		return build(module, document, true);
	}

	/**
	 * Build a domain bean within its Factory class. This will stop infinite recursion between the DataBuilder and the factory.
	 * @param module
	 * @param document
	 * @return
	 */
	public <T extends Bean> T factoryBuild(Module module, Document document) {
		return build(module, document, false);
	}

	private <T extends Bean> T build(Module module, Document document, boolean allowFactoryRecursion) {
		initialiseCycleTracking();
		return build(module, document, "", 0, allowFactoryRecursion);
	}
	
	/**
	 * Build a domain bean.
	 * @param document
	 * @return
	 */
	public <T extends Bean> T build(Document document) {
		return build(document, true);
	}
	
	/**
	 * Build a domain bean within its Factory class. This will stop infinite recursion between the DataBuilder and the factory.
	 * @param document
	 * @return
	 */
	public <T extends Bean> T factoryBuild(Document document) {
		return build(document, false);
	}

	private <T extends Bean> T build(Document document, boolean allowFactoryRecursion) {
		Module m = customer.getModule(document.getOwningModuleName());
		initialiseCycleTracking();
		return build(m, document, "", 0, allowFactoryRecursion);
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
													String binding,
													int currentDepth,
													boolean allowFactoryRecursion) {
		T result = null;
		try {
			// call the data factory if top level factory invocation is on or we have recursed already
			if ((currentDepth > 0) || allowFactoryRecursion) {
				result = dataFactory((DocumentImpl) document, currentDepth);
			}

			// create an random bean recursively, if we have none from a factory
			if (result == null) {
				if (trace) {
					trace(new StringBuilder(128).append("Creating a random bean for document ").append(module.getName()).append('.').append(document.getName()), currentDepth);
				}
				result = randomBean(module, document);

				String resultBizId = result.getBizId();
				if (visitedBizIds.contains(resultBizId)) {
					return result;
				}
				visitedBizIds.add(resultBizId);

				for (Attribute attribute : document.getAllAttributes(customer)) {
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
							String newBinding = binding.isEmpty() ? name : BindUtil.createCompoundBinding(binding, name);
							if (trace) {
								trace(new StringBuilder(128).append("Set \"").append(newBinding).append("\" to new data"), currentDepth + 1);
							}
							BindUtil.set(result,
											name,
											build(associationModule, associationDocument, newBinding, currentDepth + 1, allowFactoryRecursion));
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
		
							Document collectionDocument = collectionModule.getDocument(customer, collection.getDocumentName());
							for (int i = 0; i < cardinality; i++) {
								String newBinding = binding.isEmpty() ? name : BindUtil.createIndexedBinding(BindUtil.createCompoundBinding(binding, name), i);
								if (trace) {
									trace(new StringBuilder(128).append("Set \"").append(newBinding).append("\" to new data"), currentDepth + 1);
								}
								Bean element = build(collectionModule, collectionDocument, newBinding, currentDepth + 1, allowFactoryRecursion);
								BindUtil.addElement(result, name, element);
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
		
		for (Attribute attribute : document.getAllAttributes(customer)) {
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
						BindUtil.convertAndSet(result, name, TestUtil.randomDecimal(attribute));
						break;
					case integer:
					case longInteger:
						BindUtil.convertAndSet(result, name, TestUtil.randomInteger(attribute));
						break;
					case enumeration:
						// pick a random value from the enum
						@SuppressWarnings("unchecked")
						Class<Enum<?>> clazz = (Class<Enum<?>>) Binder.getPropertyType(result, name);
						BindUtil.set(result, name, TestUtil.randomEnum(clazz, null));
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
						BindUtil.set(result, name, TestUtil.randomText(user.getCustomerName(), module, document, attribute));
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
	
	@SuppressWarnings("unchecked")
	private <T extends Bean> T dataFactory(DocumentImpl document, int currentDepth)
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
				if (trace && methods.isEmpty()) {
					if (fixture == null) {
						trace(new StringBuilder(128).append("WARNING: ").append(factory.getClass().getName()).append(" found but there were no methods without a fixture"), currentDepth);
					}
					else {
						trace(new StringBuilder(128).append("WARNING: ").append(factory.getClass().getName()).append(" found but there were no methods for fixture ").append(fixture).append(" and no methods without a fixture"), currentDepth);
					}
				}
			}
			
			// If there are any valid methods at all
			if (! methods.isEmpty()) {
				Method method = methods.get(RANDOM.nextInt(methods.size()));
				if (trace) { 
					trace(new StringBuilder(128).append("Invoking method ").append(factory.getClass().getName()).append("::").append(method.getName()).append(" for fixture ").append(fixture), currentDepth);
				}
				result = (T) methods.get(RANDOM.nextInt(methods.size())).invoke(factory);
			}
		}
		
		return result;
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
	
	private static void trace(@SuppressWarnings("hiding") StringBuilder trace, int currentDepth) {
		for (int i = 0; i < currentDepth; i++) {
			trace.insert(0, "    ");
		}
		System.out.println(trace.toString());
	}
}
