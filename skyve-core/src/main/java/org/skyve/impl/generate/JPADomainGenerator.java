package org.skyve.impl.generate;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TreeSet;

import org.skyve.domain.Bean;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.repository.AbstractRepository;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Persistent;
import org.skyve.metadata.model.document.Association;
import org.skyve.metadata.model.document.Association.AssociationType;
import org.skyve.metadata.model.document.Collection;
import org.skyve.metadata.model.document.Collection.CollectionType;
import org.skyve.metadata.model.document.Collection.Ordering;
import org.skyve.metadata.model.document.Condition;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.Reference;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.Module.DocumentRef;

public final class JPADomainGenerator extends DomainGenerator {
	JPADomainGenerator(boolean debug,
						boolean multiTenant,
						AbstractRepository repository,
						DialectOptions dialectOptions,
						String srcPath,
						String generatedSrcPath,
						String testPath,
						String generatedTestPath,
						String[] excludedModules) {
		super(true, debug, multiTenant, repository, dialectOptions, srcPath, generatedSrcPath, testPath, generatedTestPath, excludedModules);
	}

	@Override
	public void generate() throws Exception {
		for (String customerName : repository.getAllCustomerNames()) {
			Customer customer = repository.getCustomer(customerName);

			for (Module module : customer.getModules()) {
				for (Entry<String, DocumentRef> entry : module.getDocumentRefs().entrySet()) {
					String documentName = entry.getKey();
					DocumentRef ref = entry.getValue();

					Document document = module.getDocument(customer, documentName);
					// do it for persistent and transient documents defined in this module - not external references
					if (// ((ref.getRelatedTo() != null) ||
							ref.getOwningModuleName().equals(module.getName()))// )
					{
						String packagePath = repository.MODULES_NAMESPACE + 
												module.getName() + '/' +
												repository.DOMAIN_NAME;
						File domainFolder = new File(srcPath + packagePath + '/');
						domainFolder.delete();
						domainFolder.mkdir();

						File classFile = new File(srcPath + packagePath + '/' + documentName + ".java");
						classFile.delete();
						classFile.createNewFile();

						try (FileWriter fw = new FileWriter(classFile)) {
							generateJavaFile(customer, 
												module, 
												document, 
												fw, 
												packagePath.replace('/', '.'),
												documentName);
						}
					}
				}
			}
		}
	}

	/**
	 * @param fw
	 * @param packagePath
	 * @param className
	 */
	public void generateJavaFile(Customer customer,
									Module module,
									Document document,
									FileWriter fw,
									String packagePath,
									String documentName) 
	throws IOException {
		if (debug) UtilImpl.LOGGER.info(packagePath + '.' + documentName);
		Persistent persistent = document.getPersistent();
		fw.append("package ").append(packagePath).append(";\n\n");

		Set<String> imports = new TreeSet<>();
		if (persistent != null) {
			imports.add("javax.persistence.Entity");
			imports.add("javax.persistence.Table");
		}

		StringBuilder statics = new StringBuilder(1024);
		StringBuilder attributes = new StringBuilder(1024);
		StringBuilder methods = new StringBuilder(2048);

		for (String referenceName : document.getReferenceNames()) {
			Reference reference = document.getReferenceByName(referenceName);

			String propertyClassName = reference.getDocumentName();
			String propertyPackageName = module.getDocument(customer, propertyClassName).getOwningModuleName();
			String name = reference.getName();

			String propertyPackagePath = "modules." + propertyPackageName + ".domain";
			if (! propertyPackagePath.equals(packagePath)) {
				imports.add(propertyPackagePath + '.' + propertyClassName);
			}

			String methodName = name.substring(0, 1).toUpperCase() + name.substring(1);

			if (reference instanceof Collection) {
				Collection collection = (Collection) reference;

				if ((persistent != null) && (collection.isPersistent())) {
					imports.add("javax.persistence.CascadeType");

					CollectionType type = collection.getType();
					if (type == CollectionType.child) {
						imports.add("javax.persistence.OneToMany");
						imports.add("javax.persistence.JoinColumn");

						attributes.append("\t@OneToMany(cascade = CascadeType.ALL)\n");
						attributes.append("\t@JoinColumn(name = \"parent_id\")\n");
						attributes.append("\t@org.hibernate.annotations.Cascade({org.hibernate.annotations.CascadeType.DELETE_ORPHAN})\n");
					}
					else if (type == CollectionType.composition) {
						imports.add("javax.persistence.OneToMany");

						attributes.append("\t@OneToMany(cascade = CascadeType.ALL)\n");
						attributes.append("\t@org.hibernate.annotations.Cascade({org.hibernate.annotations.CascadeType.DELETE_ORPHAN})\n");
					}
					else if (type == CollectionType.aggregation) {
						imports.add("javax.persistence.ManyToMany");
						attributes.append("\t@ManyToMany(cascade = {CascadeType.PERSIST, CascadeType.REFRESH, CascadeType.MERGE})\n");
					}

					if (! collection.getOrdering().isEmpty()) {
						imports.add("javax.persistence.OrderBy");

						attributes.append("\t@OrderBy(\"");
						for (Ordering ordering : collection.getOrdering()) {
							attributes.append(ordering.getBy());
							if (SortDirection.descending.equals(ordering.getSort())) {
								attributes.append(" desc, ");
							}
							else {
								attributes.append(" asc, ");
							}
						}
						attributes.setLength(attributes.length() - 2);
						attributes.append("\")\n");
					}
				}
				else {
					imports.add("javax.persistence.Transient");
					attributes.append("\t@Transient\n");
				}

				imports.add("java.util.List");
				imports.add("java.util.ArrayList");

				attributes.append("\tprivate List<").append(propertyClassName).append("> ").append(name);
				attributes.append(" = new ArrayList<").append(propertyClassName).append(">();\n");

				// Accessor method
				methods.append("\tpublic List<").append(propertyClassName).append("> get").append(methodName).append("() {\n");
				methods.append("\t\treturn ").append(name).append(";\n");
				methods.append("\t}\n\n");
			}
			else // this is an association
			{
				if (AssociationType.aggregation.equals(((Association) reference).getType())) {
					if (persistent != null) {
						imports.add("javax.persistence.CascadeType");
						imports.add("javax.persistence.OneToOne");
						attributes.append("\t@OneToOne(cascade = {CascadeType.PERSIST, CascadeType.REFRESH, CascadeType.MERGE})\n");
					}
					else {
						imports.add("javax.persistence.Transient");
						attributes.append("\t@Transient\n");
					}

					attributes.append("\tprivate ").append(propertyClassName).append(" ").append(name);
					attributes.append(" = null;\n");

					// Accessor method
					methods.append("\n\tpublic ").append(propertyClassName).append(" get").append(methodName).append("() {\n");
					methods.append("\t\treturn ").append(name).append(";\n");
					methods.append("\t}\n\n");

					// Mutator method
					methods.append("\tpublic void set").append(methodName).append('(');
					methods.append(propertyClassName).append(' ').append(name).append(") {\n");
					methods.append("\t\tthis.").append(name).append(" = ").append(name).append(";\n");
					methods.append("\t}\n");
				}
			}
		}

		// Document and module names

		statics.append("\tpublic static final String MODULE_NAME = \"").append(module.getName()).append("\";\n");
		statics.append("\tpublic static final String DOCUMENT_NAME = \"").append(document.getName()).append("\";\n");

		methods.append("\n\tpublic String getBizModule() {\n");
		methods.append("\t\treturn MODULE_NAME;\n");
		methods.append("\t}\n");

		methods.append("\n\tpublic String getBizDocument() {\n");
		methods.append("\t\treturn DOCUMENT_NAME;\n");
		methods.append("\t}\n");

		String bizKeyMethodCode = ((DocumentImpl) document).getBizKeyMethodCode();
		if (bizKeyMethodCode != null) {
			methods.append("\n\t@Override");
			methods.append("\n\tpublic String getBizKey() {\n");
			methods.append(bizKeyMethodCode);
			methods.append("\t}\n");
		}

		for (Attribute attribute : document.getAttributes()) {
			if (attribute.getName().equals(Bean.BIZ_KEY)) {
				continue;
			}

			statics.append("\tpublic static final String ").append(attribute.getName()).append("PropertyName = \"");
			statics.append(attribute.getName()).append("\";\n");

			if (attribute instanceof Reference) {
				continue;
			}

			// Generate imports

			Class<?> propertyClass = attribute.getAttributeType().getImplementingType();
			String propertyClassName = propertyClass.getName();
			String propertySimpleClassName = propertyClass.getSimpleName();
			String name = attribute.getName();
			String methodName = name.substring(0, 1).toUpperCase() + name.substring(1);
			boolean attributeIsPersistent = attribute.isPersistent();

			if ( !propertyClassName.startsWith("java.lang")) {
				imports.add(propertyClassName);
			}

			// Generate attributes, accessors and mutators
			if ((persistent == null) || (! attributeIsPersistent)) {
				imports.add("javax.persistence.Transient");

				attributes.append("\t@Transient\n");
			}
			if (propertySimpleClassName.equals(DECIMAL2) || 
					propertySimpleClassName.equals(DECIMAL5) ||
					propertySimpleClassName.equals(DATE_ONLY) || 
					propertySimpleClassName.equals(DATE_TIME) ||
					propertySimpleClassName.equals(TIME_ONLY) || 
					propertySimpleClassName.equals(TIMESTAMP)) {
				imports.add("org.hibernate.annotations.Type");
				attributes.append("\t@Type(type=\"").append(propertySimpleClassName).append("\")\n");
			}

			attributes.append("\tprivate ").append(propertySimpleClassName).append(' ').append(name).append(";\n");

			// Accessor method
			methods.append("\n\tpublic ").append(propertySimpleClassName).append(" get").append(methodName).append("() {\n");
			methods.append("\t\treturn ").append(name).append(";\n");
			methods.append("\t}\n\n");

			// Mutator method
			methods.append("\tpublic void set").append(methodName).append('(');
			methods.append(propertySimpleClassName).append(' ').append(name).append(") {\n");
			methods.append("\t\tthis.").append(name).append(" = ").append(name).append(";\n");
			methods.append("\t}\n");
		}

		String parentDocumentName = document.getParentDocumentName();
		Document parentDocument = null;

		// Add extra imports required
		if (persistent != null) {
			imports.add("org.skyve.impl.domain.AbstractPersistentBean");

			if (parentDocumentName != null) {
				imports.add("org.skyve.impl.domain.ChildBean");

				parentDocument = module.getDocument(customer, parentDocumentName);
				if (parentDocument.getPersistent() == null) {
					imports.add("javax.persistence.Transient");
				}
				else {
					imports.add("javax.persistence.ManyToOne");
					imports.add("javax.persistence.JoinColumn");
				}
			}
		}
		else {
			imports.add("org.skyve.impl.domain.AbstractTransientBean");

			if (parentDocumentName != null) {
				parentDocument = module.getDocument(customer, parentDocumentName);
				imports.add("org.skyve.impl.domain.ChildBean");
			}
		}

		// Add conditions
		Map<String, Condition> conditions = ((DocumentImpl) document).getConditions();
		if (conditions != null) {
			for (String conditionName : conditions.keySet()) {
				Condition condition = conditions.get(conditionName);

				methods.append("\n\tpublic boolean is").append(Character.toUpperCase(conditionName.charAt(0)));
				methods.append(conditionName.substring(1)).append("() {\n");
				methods.append("\t\treturn (").append(condition.getExpression()).append(");\n");
				methods.append("\t}\n\n");

				methods.append("\tpublic boolean isNot").append(Character.toUpperCase(conditionName.charAt(0)));
				methods.append(conditionName.substring(1)).append("() {\n");
				methods.append("\t\treturn (! is").append(Character.toUpperCase(conditionName.charAt(0)));
				methods.append(conditionName.substring(1)).append("());\n");
				methods.append("\t}\n");
			}
		}

		for (String importClassName : imports) {
			fw.append("import ").append(importClassName).append(";\n");
		}

		// generate class body
		if (persistent != null) {
			fw.append("\n@Entity(name = \"").append(document.getOwningModuleName()).append(documentName).append("\")\n");
			fw.append("@Table(name = \"").append(persistent.getName());
			String schemaName = persistent.getSchema();
			if (schemaName != null) {
			    fw.append("\" schema=\"").append(schemaName);
			}
			String catalogName = persistent.getCatalog();
			if (catalogName != null) {
			    fw.append("\" catalog=\"").append(catalogName);
			}
			fw.append("\")\n");

			fw.append("public class ").append(documentName).append(" extends AbstractPersistentBean");

			if (parentDocument != null) {
				fw.append(" implements ChildBean<").append(parentDocumentName).append("> {\n");

				// Only persist parent id if parent document is persistent
				if (parentDocument.getPersistent() != null) {
					// parent Attribute annotations
					attributes.append("\t@ManyToOne\n");
					attributes.append("\t@JoinColumn(name = \"parent_id\", insertable = false, updatable = false)\n");
				}
				else {
					attributes.append("\t@Transient\n");
				}
			}
			else {
				fw.append(" {\n");
			}
		}
		else {
			fw.append("\npublic class ").append(documentName).append(" extends AbstractTransientBean");

			if (parentDocumentName != null) {
				fw.append(" implements ChildBean<").append(parentDocumentName).append("> {\n");
			}
			else {
				fw.append(" {\n");
			}
		}

		if (parentDocumentName != null) {
			attributes.append("\tprivate ").append(parentDocumentName).append(" parent;\n\n");

			// Accessor method
			methods.append("\tpublic ").append(parentDocumentName).append(" getParent() {\n");
			methods.append("\t\treturn parent;\n");
			methods.append("\t}\n");

			// Mutator method
			methods.append("\tpublic void setParent(");
			methods.append(parentDocumentName).append(" parent) {\n");
			methods.append("\t\tthis.parent = parent;\n");
			methods.append("\t}\n");
		}

		fw.append(statics).append('\n');
		fw.append(attributes);
		fw.append(methods);

		fw.append("}\n");
	}
}
