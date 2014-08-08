package modules;

import java.util.Map.Entry;

import org.skyve.wildcat.metadata.repository.LocalDesignRepository;
import org.skyve.wildcat.metadata.repository.AbstractRepository;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.Module.DocumentRef;

/**
 * Report on metadata attributes which are marked as deprecated - 
 * 
 * Output to System.out - developer only usage
 * 
 * @author Matt Williams
 *
 */
public abstract class DeprecationReport {

	protected static String SRC_PATH; 
	
	public static String getDeprecationReport(AbstractRepository repository,String customerName) 
	throws Exception{

		StringBuilder sb = new StringBuilder();
		
		Customer customer = repository.getCustomer(customerName);
		for (Module module : customer.getModules()) {
			for (Entry<String, DocumentRef> entry : module.getDocumentRefs().entrySet()) {
				String documentName = entry.getKey();
				Document document = module.getDocument(customer, documentName);
				for(Attribute attribute: document.getAttributes()){
					if(attribute.isDeprecated()){
						sb.append("\n").append(customerName).append(".");
						sb.append(module.getName()).append(".").append(documentName);
						sb.append(".").append(attribute.getName());
					}
				}
			}
		}
		
		return sb.toString();
	}
	
	public abstract void generate() throws Exception;

	public static void main(String[] args) throws Exception {

		StringBuilder sb = new StringBuilder();
		
		AbstractRepository repository = new LocalDesignRepository();
		AbstractRepository.set(repository);
		sb.append("Attributes marked as deprecated:");
		for (String customerName : repository.getAllCustomerNames()) {
			sb.append(getDeprecationReport(repository, customerName));
		}
		
		System.out.println(sb.toString());
	}
}
