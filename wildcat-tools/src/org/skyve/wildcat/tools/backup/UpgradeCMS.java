package org.skyve.wildcat.tools.backup;

public class UpgradeCMS {
	public static void main(String[] args) 
	throws Exception {
		Backup.main(new String[] {"backup", "bizhub", args[0], args[1], args[2], args[3]});
		Backup.main(new String[] {"backup", "demo", args[0], args[1], args[2], args[3]});
		Backup.main(new String[] {"backup", "hillydale", args[0], args[1], args[2], args[3]});
		Backup.main(new String[] {"backup", "jimmartin", args[0], args[1], args[2], args[3]});
		Backup.main(new String[] {"backup", "karuit", args[0], args[1], args[2], args[3]});
		Backup.main(new String[] {"backup", "modcost", args[0], args[1], args[2], args[3]});
		Backup.main(new String[] {"backup", "transfield", args[0], args[1], args[2], args[3]});

		Backup.main(new String[] {"truncate", "bizhub", args[0], args[1], args[2], args[3]});
		Backup.main(new String[] {"truncate", "demo", args[0], args[1], args[2], args[3]});
		Backup.main(new String[] {"truncate", "hillydale", args[0], args[1], args[2], args[3]});
		Backup.main(new String[] {"truncate", "jimmartin", args[0], args[1], args[2], args[3]});
		Backup.main(new String[] {"truncate", "karuit", args[0], args[1], args[2], args[3]});
		Backup.main(new String[] {"truncate", "modcost", args[0], args[1], args[2], args[3]});
		Backup.main(new String[] {"truncate", "transfield", args[0], args[1], args[2], args[3]});
		
		Backup.main(new String[] {"restore", "bizhub", args[0], args[1], args[2], args[3]});
		Backup.main(new String[] {"restore", "demo", args[0], args[1], args[2], args[3]});
		Backup.main(new String[] {"restore", "hillydale", args[0], args[1], args[2], args[3]});
		Backup.main(new String[] {"restore", "jimmartin", args[0], args[1], args[2], args[3]});
		Backup.main(new String[] {"restore", "karuit", args[0], args[1], args[2], args[3]});
		Backup.main(new String[] {"restore", "modcost", args[0], args[1], args[2], args[3]});
		Backup.main(new String[] {"restore", "transfield", args[0], args[1], args[2], args[3]});
/*
		Backup.main(new String[] {"reindex", "bizhub", args[0], args[1], args[2], args[3]});
		Backup.main(new String[] {"reindex", "demo", args[0], args[1], args[2], args[3]});
		Backup.main(new String[] {"reindex", "hillydale", args[0], args[1], args[2], args[3]});
		Backup.main(new String[] {"reindex", "jimmartin", args[0], args[1], args[2], args[3]});
		Backup.main(new String[] {"reindex", "karuit", args[0], args[1], args[2], args[3]});
		Backup.main(new String[] {"reindex", "modcost", args[0], args[1], args[2], args[3]});
		Backup.main(new String[] {"reindex", "transfield", args[0], args[1], args[2], args[3]});
*/
	}
}
