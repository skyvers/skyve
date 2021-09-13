package org.skyve.impl.generate;

import java.io.File;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import org.skyve.impl.metadata.repository.AbstractRepository;
import org.skyve.impl.metadata.repository.LocalDesignRepository;
import org.skyve.impl.metadata.repository.module.BizQLMetaData;
import org.skyve.impl.metadata.repository.module.MetaDataQueryColumnMetaData;
import org.skyve.impl.metadata.repository.module.MetaDataQueryContentColumnMetaData;
import org.skyve.impl.metadata.repository.module.MetaDataQueryMetaData;
import org.skyve.impl.metadata.repository.module.MetaDataQueryProjectedColumnMetaData;
import org.skyve.impl.metadata.repository.module.ModuleMetaData;
import org.skyve.impl.metadata.repository.module.QueryMetaData;
import org.skyve.impl.metadata.repository.module.SQLMetaData;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Persistent;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.BizQLDefinition;
import org.skyve.metadata.module.query.MetaDataQueryColumn;
import org.skyve.metadata.module.query.MetaDataQueryContentColumn;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.module.query.QueryDefinition;
import org.skyve.metadata.module.query.SQLDefinition;

public class QueryGenerator {
	private QueryGenerator() {
		// do nothing
	}

	public static List<QueryDefinition> generate(Customer customer, Module module, boolean includeAssociationBizKeys) {
		Set<String> documentNames = module.getDocumentRefs().keySet();
		List<QueryDefinition> result = new ArrayList<>(documentNames.size());

		for (String documentName : documentNames) {
			Document document = module.getDocument(customer, documentName);
			Persistent persistent = document.getPersistent();
			if ((persistent != null) && (persistent.getName() != null)) { // transient document
				result.add(module.getDocumentDefaultQuery(customer, documentName, includeAssociationBizKeys));
			}
		}

		return result;
	}

	public static String generateQueryXML(Customer customer, Module module, boolean includeAssociationBizKeys) {
		ModuleMetaData newModule = new ModuleMetaData();

		List<QueryMetaData> metaDataQueries = newModule.getQueries();
		List<QueryDefinition> queries = generate(customer, module, includeAssociationBizKeys);
		for (QueryDefinition query : queries) {
			if (query instanceof MetaDataQueryDefinition) {
				MetaDataQueryDefinition documentQuery = (MetaDataQueryDefinition) query;
				MetaDataQueryMetaData documentQueryMetaData = new MetaDataQueryMetaData();
				documentQueryMetaData.setName(documentQuery.getName());
				documentQueryMetaData.setDescription(documentQuery.getDescription());
				documentQueryMetaData.setDocumentName(documentQuery.getDocumentName());
				documentQueryMetaData.setDocumentation(documentQuery.getDocumentation());
	
				List<MetaDataQueryColumnMetaData> metaDataColumns = documentQueryMetaData.getColumns();
				for (MetaDataQueryColumn queryColumn : documentQuery.getColumns()) {
					MetaDataQueryColumnMetaData metaDataColumn = null;
					if (queryColumn instanceof MetaDataQueryContentColumn) {
						MetaDataQueryContentColumn contentQueryColumn = (MetaDataQueryContentColumn) queryColumn;
						MetaDataQueryContentColumnMetaData metaDataContentColumn = new MetaDataQueryContentColumnMetaData();
						metaDataColumn = metaDataContentColumn;
						metaDataContentColumn.setDisplay(contentQueryColumn.getDisplay());
						metaDataContentColumn.setPixelWidth(contentQueryColumn.getPixelWidth());
						metaDataContentColumn.setPixelHeight(contentQueryColumn.getPixelHeight());
					}
					else {
						metaDataColumn = new MetaDataQueryProjectedColumnMetaData();
					}
					metaDataColumn.setBinding(queryColumn.getBinding());
					metaDataColumn.setDisplayName(queryColumn.getDisplayName());
					metaDataColumn.setName(queryColumn.getName());
					metaDataColumn.setSortOrder(queryColumn.getSortOrder());
					if (queryColumn.isHidden()) {
						metaDataColumn.setHidden(Boolean.TRUE);
					}
					metaDataColumns.add(metaDataColumn);
				}
				
				int timeoutInSeconds = documentQuery.getTimeoutInSeconds();
				if (timeoutInSeconds > 0) {
					documentQueryMetaData.setTimeoutInSeconds(Integer.valueOf(timeoutInSeconds));
				}
				metaDataQueries.add(documentQueryMetaData);
			}
			else if (query instanceof SQLDefinition) {
				SQLDefinition sql = (SQLDefinition) query;
				SQLMetaData sqlMetaData = new SQLMetaData();
				sqlMetaData.setName(sql.getName());
				sqlMetaData.setDescription(sql.getDescription());
				sqlMetaData.setDocumentation(sql.getDocumentation());
				sqlMetaData.setQuery(sql.getQuery());
				int timeoutInSeconds = sql.getTimeoutInSeconds();
				if (timeoutInSeconds > 0) {
					sqlMetaData.setTimeoutInSeconds(Integer.valueOf(timeoutInSeconds));
				}
				metaDataQueries.add(sqlMetaData);
			}
			else if (query instanceof BizQLDefinition) {
				BizQLDefinition bizQL = (BizQLDefinition) query;
				BizQLMetaData bizQLMetaData = new BizQLMetaData();
				bizQLMetaData.setName(bizQL.getName());
				bizQLMetaData.setDescription(bizQL.getDescription());
				bizQLMetaData.setDocumentation(bizQL.getDocumentation());
				bizQLMetaData.setQuery(bizQL.getQuery());
				int timeoutInSeconds = bizQL.getTimeoutInSeconds();
				if (timeoutInSeconds > 0) {
					bizQLMetaData.setTimeoutInSeconds(Integer.valueOf(timeoutInSeconds));
				}
				metaDataQueries.add(bizQLMetaData);
			}
		}
		return XMLMetaData.marshalModule(newModule, false);
	}
	
	public static void main(String[] args) throws Exception {
		String customerName = null;
		String moduleName = null;
		boolean includeAssociationBizKeys = false;
		
		if (args.length == 2) {
			customerName = args[0];
			moduleName = args[1];
		}
		else if (args.length == 3) {
			customerName = args[0];
			moduleName = args[1];
			includeAssociationBizKeys = Boolean.parseBoolean(args[2]);
		}
		else {
			System.err.println("Usage: org.skyve.impl.generate.QueryGenerator customerName moduleName");
			System.exit(1);
		}

		AbstractRepository.set(new LocalDesignRepository());
		AbstractRepository repository = AbstractRepository.get();
		Customer customer = repository.getCustomer(customerName);
		Module module = repository.getModule(customer, moduleName);
		File file = new File("./generatedQueries.xml");
		UtilImpl.LOGGER.info("Output is written to " + file.getCanonicalPath());
		try (PrintWriter out = new PrintWriter(file)) {
			out.println(generateQueryXML(customer, module, includeAssociationBizKeys));
			out.flush();
		}
	}
}
