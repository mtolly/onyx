using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Drawing;

namespace X360
{
    /// <summary>
    /// Resource data
    /// </summary>
    public static class PublicResources
    {
        /// <summary>
        /// Dash Image
        /// </summary>
        public static Image DashImage { get { return global::X360.Properties.Resources.Dash; }}
        /// <summary>
        /// No Achievement image
        /// </summary>
        public static Image NoImage { get { return global::X360.Properties.Resources.NoImg; }}
        /// <summary>
        /// Achievement locked image
        /// </summary>
        public static Image Locked { get { return global::X360.Properties.Resources.Locked; }}
        /// <summary>
        /// Paw icon
        /// </summary>
        public static Icon PawIcon { get { return global::X360.Properties.Resources.paw; }}
        /// <summary>
        /// GPL License
        /// </summary>
        public static string GPL { get { return global::X360.Properties.Resources.GPL30; }}
    }
}