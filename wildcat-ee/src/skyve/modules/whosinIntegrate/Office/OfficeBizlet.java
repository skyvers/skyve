package modules.whosinIntegrate.Office;

import modules.whosinIntegrate.domain.Office;

import org.skyve.apps.ModulesUtil;
import org.skyve.metadata.model.document.Bizlet;

public class OfficeBizlet extends Bizlet<Office> {

	/**
	 * 
	 */
	private static final long serialVersionUID = -3760683794556432066L;

	/**
	 * Compile a single-line representation of the office
	 * 
	 * @param office
	 * @return
	 */
	public static String bizKey(Office office){

		StringBuilder sb = new StringBuilder();
		sb.append(ModulesUtil.concatWithDelim(" ", office.getLevelUnit(), office.getBuildingName()));
		sb.append((sb.length()>0?",":""));
		sb.append(ModulesUtil.concatWithDelim(" ", office.getStreetAddress(), office.getSuburb()));
		
		return sb.toString();
	}
}
