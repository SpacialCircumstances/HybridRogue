﻿using System;

namespace HybridRogue.Desktop
{
    /// <summary>
    /// The main class.
    /// </summary>
    public static class Program
    {
        /// <summary>
        /// The main entry point for the application.
        /// </summary>
        [STAThread]
        static void Main()
        {
            using (var game = new HybridRogue.Game.Game1())
                game.Run();
        }
    }
}
