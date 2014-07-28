package org.skyve.wildcat.generate;

import java.io.File;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Persistent;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.Query;
import org.skyve.metadata.module.query.QueryColumn;
import org.skyve.wildcat.metadata.repository.AbstractRepository;
import org.skyve.wildcat.metadata.repository.LocalDesignRepository;
import org.skyve.wildcat.metadata.repository.module.Column;
import org.skyve.wildcat.metadata.repository.module.ModuleMetaData;
import org.skyve.wildcat.metadata.repository.module.QueryMetaData;
import org.skyve.wildcat.util.UtilImpl;
import org.skyve.wildcat.util.XMLUtil;


public class QueryGenerator {
	private QueryGenerator() {
		// do nothing
	}

	public static List<Query> generate(Customer customer, Module module) 
	throws MetaDataException {
		Set<String> documentNames = module.getDocumentRefs().keySet();
		List<Query> result = new ArrayList<>(documentNames.size());

		for (String documentName : documentNames) {
			Document document = module.getDocument(customer, documentName);
			Persistent persistent = document.getPersistent();
			if ((persistent != null) && (persistent.getName() != null)) { // transient document
				result.add(module.getDocumentDefaultQuery(customer, documentName));
			}
		}

		return result;
	}

	public static String generateQueryXML(Customer customer,
											Module module)
	throws MetaDataException {
		ModuleMetaData newModule = new ModuleMetaData();

		List<Query> queries = generate(customer, module);
		for (Query query : queries) {
			QueryMetaData metaDataQuery = new QueryMetaData();
			metaDataQuery.setName(query.getName());
			metaDataQuery.setDisplayName(query.getDisplayName());
			metaDataQuery.setDescription(query.getDescription());
			metaDataQuery.setDocumentName(query.getDocumentName());
			metaDataQuery.setDocumentation(query.getDocumentation());

			for (QueryColumn queryColumn : query.getColumns()) {
				Column metaDataColumn = new Column();
				metaDataColumn.setBinding(queryColumn.getBinding());
				metaDataColumn.setDisplayName(queryColumn.getDisplayName());
				metaDataColumn.setName(queryColumn.getName());
				metaDataColumn.setSortOrder(queryColumn.getSortOrder());
				metaDataQuery.getColumns().add(metaDataColumn);
			}
			newModule.getQueries().add(metaDataQuery);
		}
		return XMLUtil.marshalModule(newModule, false);
	}
	
	public static void main(String[] args) throws Exception {
		String customerName = null;
		String moduleName = null;
		
		if (args.length == 2) {
			customerName = args[0];
			moduleName = args[1];
		}
		else {
			System.err.println("Usage: org.skyve.wildcat.generate.QueryGenerator customerName moduleName");
			System.exit(1);
		}

		AbstractRepository.set(new LocalDesignRepository());
		AbstractRepository repository = AbstractRepository.get();
		Customer customer = repository.getCustomer(customerName);
		Module module = repository.getModule(customer, moduleName);
		File file = new File("./generatedQueries.xml");
		UtilImpl.LOGGER.info("Output is written to " + file.getCanonicalPath());
		try (PrintWriter out = new PrintWriter(file)) {
			out.println(generateQueryXML(customer, module));
			out.flush();
		}
	}
}
