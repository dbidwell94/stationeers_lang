using Assets.Scripts.UI;

namespace Slang
{
    public static class SlangDocs
    {
        public static StationpediaPage[] Pages
        {
            get
            {
                return
                [
                    new StationpediaPage(
                        "slang-init",
                        "Slang",
                        "Slang is a new high level language built specifically for Stationeers"
                    ),
                ];
            }
        }
    }
}
